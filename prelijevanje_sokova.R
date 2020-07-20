library(tidyverse)
library(lubridate)
library(readxl)
library(scales)
library(ecb)

# Funkcija za kopiranje u excel ####
skopiraj <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

# Ggplot tema #### 

gtema <- theme_minimal() + theme(panel.background = element_rect(fill="#e7eaf6",linetype = 0),plot.background = element_rect(fill="#e7eaf6",linetype = 0),legend.box.background = element_rect(fill="#e7eaf6",linetype = 0),text = element_text(colour = "#000000"),plot.caption = element_text(hjust = 0),legend.position = "top",legend.title = element_blank(),panel.border = element_blank(),axis.line = element_blank(),panel.grid.major = element_line(size = 0.5, linetype = "dotted",colour = "#233142"))
theme_set(gtema)

# paleta boja
boje_fill <- scale_fill_manual(values = c("#155e63","#e84545","#25a55f","#ffc93c","#9b5d73","#ff7c38","#7e6752","#679186","#2e99b0","#01d28e","#cd8d7b","#bbbbbb","#f7be16","#b5525c","#4f81c7","#ff8a5c","#32ff6a","#393e46","#df0054","#f69314"))
boje_col <- scale_color_manual(values = c("#155e63","#e84545","#25a55f","#ffc93c","#9b5d73","#ff7c38","#7e6752","#679186","#2e99b0","#01d28e","#cd8d7b","#bbbbbb","#f7be16","#b5525c","#4f81c7","#ff8a5c","#32ff6a","#393e46","#df0054","#f69314"))

# 1. Učitavanje podataka ####
load("Z:/DSR/DWH/nav_opce.Rda")
nav_opce <- nav
load("Z:/DSR/DWH/imovina_S2.Rda")
load("Z:/DSR/DWH/NAV.Rda")

# 2. Prikaz udjela raznih klasa imovine ####

# Struktura ulaganja - IF
pom <- nav %>% filter(izvjestaj=="NAV" & vrsta0=="Investicijski") %>% group_by(datum,razina2) %>% summarise(iznos=sum(iznos,na.rm=T))
ggplot(pom,aes(x=datum,y=iznos,fill=razina2)) + geom_col(position="fill") + boje_fill
# Struktura ulaganja - MF
pom <- nav %>% filter(izvjestaj=="NAV" & vrsta0=="Mirovinski") %>% group_by(datum,razina2) %>% summarise(iznos=sum(iznos,na.rm=T))
ggplot(pom,aes(x=datum,y=iznos,fill=razina2)) + geom_col(position="fill") + boje_fill
# Struktura ulaganja - OS
pom <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo") %>% group_by(datum,razina1) %>% summarise(iznos=sum(iznos,na.rm=T))
ggplot(pom,aes(x=datum,y=iznos,fill=razina1)) + geom_col(position="fill") + boje_fill

# Udio dionica i obveznica u portfelju osiguranja i fondova
pom11 <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo" & razina1 %in% c("Equity","Corporate bonds","Government bonds")) %>% mutate(razina1=case_when(razina1=="Equity"~"Dionice",T~"Obveznice")) %>% group_by(datum,razina1) %>% summarise(iznos=sum(iznos,na.rm=T))
pom12 <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo") %>% group_by(datum) %>% summarise(ukupno=sum(iznos,na.rm=T))
pom1 <- left_join(pom11,pom12,by="datum") %>% mutate(vrsta0="Osiguranja",udio=iznos/ukupno*100)
pom21 <- nav %>% filter(izvjestaj=="NAV" & razina2 %in% c("Dionice","Dugoročni dužnički vrijednosni papiri")) %>% mutate(razina1=case_when(razina2=="Dionice"~"Dionice",T~"Obveznice")) %>% group_by(datum,razina1,vrsta0) %>% summarise(iznos=sum(iznos,na.rm=T))
pom22 <- nav %>% filter(izvjestaj=="NAV") %>% group_by(datum,vrsta0) %>% summarise(ukupno=sum(iznos,na.rm=T))
pom2 <- left_join(pom21,pom22,by=c("datum","vrsta0")) %>% mutate(udio=iznos/ukupno*100)
pom <- rbind(pom1,pom2)
ggplot(pom,aes(x=datum,y=udio,fill=razina1)) + geom_col() + facet_wrap(~vrsta0) + boje_fill
# pom <- pom %>% filter(datum=="2020-03-31" | datum=="2020-06-30") %>% select(datum,vrsta0,razina1,udio) %>% spread(vrsta0,udio)

# Udio investicijskih fondova u portfelju osiguranja i fondova
pom11 <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo" & razina1 %in% c("Investment Funds")) %>% group_by(datum) %>% summarise(iznos=sum(iznos,na.rm=T))
pom12 <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo") %>% group_by(datum) %>% summarise(ukupno=sum(iznos,na.rm=T))
pom1 <- left_join(pom11,pom12,by="datum") %>% mutate(vrsta0="Osiguranja",udio=iznos/ukupno*100)
pom21 <- nav %>% filter(izvjestaj=="NAV" & razina2 %in% c("Investicijski fondovi")) %>% group_by(datum,vrsta0) %>% summarise(iznos=sum(iznos,na.rm=T))
pom22 <- nav %>% filter(izvjestaj=="NAV") %>% group_by(datum,vrsta0) %>% summarise(ukupno=sum(iznos,na.rm=T))
pom2 <- left_join(pom21,pom22,by=c("datum","vrsta0")) %>% mutate(udio=iznos/ukupno*100)
pom <- rbind(pom1,pom2)
ggplot(pom,aes(x=datum,y=udio,fill=vrsta0)) + geom_col() + facet_wrap(~vrsta0) + boje_fill
# pom <- pom %>% filter(datum=="2020-03-31" | datum=="2020-06-30") %>% select(datum,vrsta0,udio) %>% spread(vrsta0,udio)

# Udio nekretnina i kredita u portfelju osiguranja i fondova
pom11 <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo" & razina1 %in% c("Property","Mortgages and loans")) %>% group_by(datum) %>% summarise(iznos=sum(iznos,na.rm=T))
pom12 <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo") %>% group_by(datum) %>% summarise(ukupno=sum(iznos,na.rm=T))
pom1 <- left_join(pom11,pom12,by="datum") %>% mutate(vrsta0="Osiguranja",udio=iznos/ukupno*100)
pom21 <- nav %>% filter(izvjestaj=="NAV" & razina2 %in% c("Nekretnine")) %>% group_by(datum,vrsta0) %>% summarise(iznos=sum(iznos,na.rm=T))
pom22 <- nav %>% filter(izvjestaj=="NAV") %>% group_by(datum,vrsta0) %>% summarise(ukupno=sum(iznos,na.rm=T))
pom2 <- left_join(pom21,pom22,by=c("datum","vrsta0")) %>% mutate(udio=iznos/ukupno*100)
pom <- rbind(pom1,pom2)
ggplot(pom,aes(x=datum,y=udio,fill=vrsta0)) + geom_col() + facet_wrap(~vrsta0) + boje_fill
rm(pom11,pom12,pom1,pom21,pom22,pom2)

# 3. Inputi - kalibracija scenarija ####

# 3.1. Neto uplate u mirovinske fondove ( u posljednjih 6 mjeseci 2020.)
doprinosi_omf <- 2994982831.33
doprinosi_dmf <- 213532281.91

# 3.2. OBVEZNICE - rast prinosa do kraja godine
yld_sok <- 0.01

# 3.3. DIONICE - razina 6-mjesečnog VaR-a koju uzimamo za svaku pojedinu dionicu
kvantil_dionice <- 0.05

# 3.4. INVESTICIJSKI FONDOVI - razina 6-mjesečnog VaR-a koji uzimamo za svaki pojedini fond
kvantil_ifondovi <- 0.05

# 3.5. NEKRETNINE - promjena cijena na godišnjoj razini
delta_nekret <- -0.115

# 3.6. TEČAJ - kvantil polugodišnje promjene tečaja kojeg stresiramo (0.1 - aprecijacija, 0.9 - deprecijacija)
kvantil_tecaj <- 0.9

# 4. Stresiranje obvezničkog portfelja ####

# 3.1. Popis obveznica - input za bloomberg
#pom1 <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo" & razina1 %in% c("Corporate bonds","Government bonds") & !is.na(iznos)) %>% select(isin) %>% distinct()
#pom2 <- nav %>% filter(izvjestaj=="NAV" & razina2=="Dugoročni dužnički vrijednosni papiri") %>% select(isin) %>% distinct()
#pom <- rbind(pom1,pom2) %>% distinct()
#xlsx::write.xlsx2(pom,file = "popis_obveznica.xlsx",append = T)
#rm(pom1,pom2,pom)

# 3.2. Izračun prinosa - osiguranja
pom1 <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo" & razina1 %in% c("Corporate bonds","Government bonds") & !is.na(iznos)) %>% select(datum,vrsta1=portfelj,subjekt,razina1,protustrana,iznos,isin,drzava,valuta,vrednovanje,dospijece,nominala,kamata,jed_cijena) %>% mutate(vrsta0="Osiguranja",vrsta2=NA,vrsta1=case_when(vrsta1=="Non-life [split applicable]"~"Neživot",vrsta1=="Life [split applicable]"~"Život",T~"Ostalo"),razina1=case_when(razina1=="Corporate bonds"~"Korporativne",razina1=="Government bonds"~"Državne"),jed_cijena=ifelse(is.na(jed_cijena),iznos/(nominala+kamata),jed_cijena))
# spajanje s podacima s bloomberga
temp <- read_excel("obveznice.xlsx",sheet = "vrijednosti",skip = 1,na = c("#N/A Field Not Applicable","#N/A Invalid Security")) %>% select(isin,kupon=CPN,frekvencija=CPN_FREQ,datum_izdanja=SECURITY_PRICING_DATE,datum_dospijeca=MATURITY,kolicina_izdanje=AMT_ISSUED) %>% mutate(datum_izdanja=as.Date(datum_izdanja,"%d.%m.%Y."),datum_dospijeca=as.Date(datum_dospijeca,"%d.%m.%Y."),kupon=as.numeric(kupon)) %>% filter(!is.na(kupon)) %>% mutate(frekvencija=ifelse(is.na(frekvencija),2,frekvencija))
pom1 <- pom1 %>% left_join(temp,by="isin") %>% mutate(dospijece=case_when(is.na(dospijece)~datum_dospijeca,T~dospijece)) %>% select(-datum_dospijeca)

# Izračun prinosa
pom <- pom1 %>% mutate(T=(dospijece-datum)/365,M=nominala,n=frekvencija,P=jed_cijena*M,C=(kupon/100)*M/n,T=as.numeric(T),ytm=NA)
# traženje pozitivnog prinosa do dospijeća
for (i in 1:nrow(pom)) {
  if (pom$jed_cijena[i]==0 | pom$iznos[i]==0) { # imamo nule kod nekih obveznica pa nastane šou
    pom$ytm[i] <- 0
  } else if (is.na(pom$dospijece[i])) { # formula za prinos ako je obveznica bez dospijeća, tj. ako je perpetuity
    pom$ytm[i] <- (pom$C[i]*pom$n[i])/pom$P[i]
  } else if (pom$T[i]<=0.01) {
    pom$ytm[i] <- pom$kupon/100
  } else {
    tryCatch({
      f_cijena <- function(y) pom$C[i]/(y/pom$n[i])*(1-1/(1+y/pom$n[i])^(pom$n[i]*pom$T[i])) + pom$M[i]/(1+y/pom$n[i])^(pom$n[i]*pom$T[i]) - pom$P[i]
      pom$ytm[i] <- uniroot(f_cijena, interval = c(0.0000000001, 1), tol = 0.0000000001)$root
    }, error=function(e){})
  }
}
# popunjavanje praznih polja <- traženje negativnog yidelda
for (i in 1:nrow(pom)){
  if(is.na(pom$ytm[i])){
    tryCatch({
      f_cijena <- function(y) pom$C[i]/(y/pom$n[i])*(1-1/(1+y/pom$n[i])^(pom$n[i]*pom$T[i])) + pom$M[i]/(1+y/pom$n[i])^(pom$n[i]*pom$T[i]) - pom$P[i]
      pom$ytm[i] <- uniroot(f_cijena, interval = c(-1, -0.0000000001), tol = 0.0000000001)$root
    }, error=function(e){})
  }
}
# popunjavanje praznih polja <- traženje zadnjeg prethodnog yielda
for (i in 1:nrow(pom)){
  if(is.na(pom$ytm[i])){
    tryCatch({
      procjena <- pom %>% filter(datum<pom$datum[i] & isin==pom$isin[i]) %>% arrange(datum)
      pom$ytm[i] <- procjena$ytm[nrow(procjena)]
    }, error=function(e){})
  }
}
pom1 <- pom

# 3.3. Izračun prinosa - fondovi
pom2 <- nav %>% filter(izvjestaj=="NAV" & razina2=="Dugoročni dužnički vrijednosni papiri") %>% select(datum,vrsta0,vrsta1,vrsta2=vrsta4,subjekt,razina1=razina3,protustrana,iznos,isin,drzava,valuta,vrednovanje,dospijece,nominala_u_valuti,kamata_u_valuti,jed_cijena_u_valuti,tecaj) %>% mutate(razina1=case_when(razina1=="Korporativne"~"Korporativne",razina1 %in% c("Državne, središnjih banaka i javnih međunarodnih tijela","Municipalne","Državne, središnjih banaka i međunarodnih organizacija")~"Državne"))
temp <- read_excel("obveznice.xlsx",sheet = "vrijednosti",skip = 1,na = c("#N/A Field Not Applicable","#N/A Invalid Security")) %>% select(isin,kupon=CPN,frekvencija=CPN_FREQ,datum_izdanja=SECURITY_PRICING_DATE,datum_dospijeca=MATURITY,kolicina_izdanje=AMT_ISSUED) %>% mutate(datum_izdanja=as.Date(datum_izdanja,"%d.%m.%Y."),datum_dospijeca=as.Date(datum_dospijeca,"%d.%m.%Y."),kupon=as.numeric(kupon)) %>% filter(!is.na(kupon)) %>% mutate(frekvencija=ifelse(is.na(frekvencija),2,frekvencija))
pom2 <- pom2 %>% left_join(temp,by="isin") %>% mutate(dospijece=datum_dospijeca) %>% select(-datum_dospijeca)

# Izračun prinosa
pom <- pom2 %>% mutate(T=(dospijece-datum)/365,M=nominala_u_valuti,n=frekvencija,P=jed_cijena_u_valuti,C=(kupon/100)*M/n) %>% mutate(T=as.numeric(T),ytm=NA,P=P*M)
# traženje pozitivnog prinosa do dospijeća
for (i in 1:nrow(pom)) {
  if (pom$jed_cijena_u_valuti[i]==0 | pom$iznos[i]==0) { # imamo nule kod nekih obveznica pa nastane šou
    pom$ytm[i] <- 0
  } else if (is.na(pom$dospijece[i])) { # formula za prinos ako je obveznica bez dospijeća, tj. ako je perpetuity
    pom$ytm[i] <- (pom$C[i]*pom$n[i])/pom$P[i]
  } else if (pom$T[i]<=0.01) {
    pom$ytm[i] <- pom$kupon/100
  } else {
    tryCatch({
      f_cijena <- function(y) pom$C[i]/(y/pom$n[i])*(1-1/(1+y/pom$n[i])^(pom$n[i]*pom$T[i])) + pom$M[i]/(1+y/pom$n[i])^(pom$n[i]*pom$T[i]) - pom$P[i]
      pom$ytm[i] <- uniroot(f_cijena, interval = c(0.0000000001, 1), tol = 0.0000000001)$root
    }, error=function(e){})
  }
}
# popunjavanje praznih polja <- traženje negativnog yidelda
for (i in 1:nrow(pom)){
  if(is.na(pom$ytm[i])){
    tryCatch({
      f_cijena <- function(y) pom$C[i]/(y/pom$n[i])*(1-1/(1+y/pom$n[i])^(pom$n[i]*pom$T[i])) + pom$M[i]/(1+y/pom$n[i])^(pom$n[i]*pom$T[i]) - pom$P[i]
      pom$ytm[i] <- uniroot(f_cijena, interval = c(-1, -0.0000000001), tol = 0.0000000001)$root
    }, error=function(e){})
  }
}
# popunjavanje praznih polja <- traženje zadnjeg prethodnog yielda
for (i in 1:nrow(pom)){
  if(is.na(pom$ytm[i])){
    tryCatch({
      procjena <- pom %>% filter(datum<pom$datum[i] & isin==pom$isin[i]) %>% arrange(datum)
      pom$ytm[i] <- procjena$ytm[nrow(procjena)]
    }, error=function(e){})
  }
}
#bla <- pom %>% rowid_to_column() %>% filter(is.na(ytm))
pom2 <- pom

# 3.4. sklapanje
pom1 <- pom1 %>% select(datum,vrsta0,vrsta1,vrsta2,subjekt,razina1,protustrana,iznos,isin,drzava,valuta,vrednovanje,dospijece,nominala,kamata,jed_cijena,kupon,frekvencija,datum_izdanja,kolicina_izdanje,ytm) %>% mutate(tecaj=NA)
pom2 <- pom2 %>% select(datum,vrsta0,vrsta1,vrsta2,subjekt,razina1,protustrana,iznos,isin,drzava,valuta,vrednovanje,dospijece,nominala=nominala_u_valuti,kamata=kamata_u_valuti,jed_cijena=jed_cijena_u_valuti,tecaj,kupon,frekvencija,datum_izdanja,kolicina_izdanje,ytm)
obveznice <- rbind(pom1,pom2)
# brisanje privremenih objekata iz workspacea
rm(pom1,pom2,temp,i,f_cijena,procjena)

# 3.5. Analiza uzorka

# crtanje ukupnih prinosa na državne obveznice i raspona
#pom <- obveznice %>% filter(!is.na(ytm) & datum>="2014-01-31" & razina1 == "Državne" & drzava %in% c("HR")) %>% group_by(datum,drzava,vrsta0) %>% summarise(ytm=weighted.mean(x=ytm,w=iznos))
#pom1 <- obveznice %>% filter(datum>="2014-01-31" & drzava=="HR" & razina1 =="Državne") %>% group_by(datum,subjekt,vrsta0) %>% summarise(ytm=weighted.mean(x=ytm,w=iznos)) %>% group_by(datum,vrsta0) %>% summarise(p10=quantile(ytm,probs = 0.1,na.rm = T),p25=quantile(ytm,probs = 0.25,na.rm = T),p75=quantile(ytm,probs = 0.75,na.rm = T),p90=quantile(ytm,probs = 0.9,na.rm = T))
#ggplot(pom,aes(x=datum,y=ytm,col=drzava)) + boje_col + facet_wrap(~vrsta0) + geom_ribbon(data=pom1,inherit.aes = F,aes(x=datum,ymin=p10,ymax=p90,fill="#d9455f"),alpha=0.3,show.legend = F) + geom_ribbon(data=pom1,inherit.aes = F,aes(x=datum,ymin=p25,ymax=p75,fill="#d9455f"),alpha=0.4,show.legend = F) + geom_line(size=1.1) + scale_y_continuous(labels = percent)

# crtanje po preostaloj ročnosti
#pom <- obveznice %>% filter(!is.na(ytm) & datum>="2014-01-31" & razina1 == "Državne" & drzava %in% c("HR")) %>% mutate(rocnost=as.numeric((dospijece-datum)/365),rocnost=ifelse(rocnost<2,"1",ifelse(rocnost<4,"3",ifelse(rocnost<6,"5",ifelse(rocnost<8,"7",ifelse(rocnost<12,"10",ifelse(rocnost<17,"15","20"))))))) %>% group_by(datum,rocnost) %>% summarise(ytm=weighted.mean(x=ytm,w=iznos))
#ggplot(pom,aes(x=datum,y=ytm,col=rocnost)) + geom_line(size=1.1) + boje_col

# pokrivenost obveznica u uzorku
#pom1 <- obveznice %>% filter(!is.na(ytm)) %>% group_by(datum,vrsta0) %>% summarise(uzorak=sum(iznos,na.rm = T))
#pom2 <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo" & razina1 %in% c("Corporate bonds","Government bonds") & !is.na(iznos)) %>% group_by(datum) %>% summarise(iznos=sum(iznos,na.rm=T)) %>% mutate(vrsta0="Osiguranja")
#pom3 <- nav %>% filter(izvjestaj=="NAV" & razina2=="Dugoročni dužnički vrijednosni papiri") %>% group_by(datum,vrsta0) %>% summarise(iznos=sum(iznos,na.rm = T))
#pom <- full_join(pom1,rbind(pom2,pom3),by=c("datum","vrsta0")) %>% mutate(udio=uzorak/iznos*100)
#ggplot(pom,aes(x=datum,y=udio,col=vrsta0)) + geom_line()
#rm(pom1,pom2,pom3)

# 3.6. Simulacija vrijednosti ulaganja u obveznice

pom1 <- obveznice %>% filter((vrsta0=="Osiguranja" & datum=="2020-03-31") | (vrsta0 %in% c("Mirovinski","Investicijski") & datum=="2020-06-30")) %>% mutate(dospijece=if_else(dospijece<="2020-12-31",make_date(2021,1,31),dospijece),rocnost=as.numeric((dospijece-datum)/365),M=nominala,n=frekvencija,P=jed_cijena*M,C=(kupon/100)*M/n) %>% mutate(datum="2020-12-31",ytm=ifelse(!drzava %in% c("DE","US"),ytm+yld_sok,ytm),P1=ifelse(is.na(rocnost),C*n/ytm,C/(ytm/n)*(1-1/((1+ytm/n)^(n*rocnost))) + M/((1+ytm/n)^(n*rocnost))),iznos=ifelse(is.na(ytm),iznos,ifelse(!drzava %in% c("DE","US"),ifelse(is.na(tecaj),P1+C,(P1+C)*tecaj),iznos))) %>% select(-rocnost,-M,-n,-P,-P1,-C)
obveznice <- rbind(obveznice,pom1)
pom <- obveznice %>% group_by(datum,vrsta0) %>% summarise(iznos=sum(iznos,na.rm=T))
ggplot(pom,aes(x=datum,y=iznos)) + geom_line(size=1.1) + facet_wrap(~vrsta0,scales = "free") + scale_y_continuous(labels = scales::comma)
rm(pom1,yld_sok)

# 5. Simulacija vrijednosti dionica ####

# Popis (svih) dionica - input za bloomberg
#pom1 <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo" & razina1=="Equity") %>% select(isin) %>% distinct()
#pom2 <- nav %>% filter(izvjestaj=="NAV" & razina2=="Dionice") %>% select(isin) %>% distinct()
#pom <- rbind(pom1,pom2) %>% distinct()
#xlsx::write.xlsx2(pom,file = "popis_dionica.xlsx",append = T,sheetName = "ukupno")

# Popis dionica samo na zadnji datum - input za bloomberg
#pom1 <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo" & razina1=="Equity" & datum=="2020-03-31") %>% select(isin) %>% distinct()
#pom2 <- nav %>% filter(izvjestaj=="NAV" & razina2=="Dionice" & datum=="2020-06-30") %>% select(isin) %>% distinct()
#pom <- rbind(pom1,pom2) %>% distinct()
#xlsx::write.xlsx2(pom,file = "popis_dionica.xlsx",append = T,sheetName = "zadnji_datum")

# pokrivenost simuliranih dionica u uzorku
#temp <- read_excel("dionice_zadnji_datum.xlsx",sheet = "vrijednosti_mjesecni",skip = 5,na = c("#N/A N/A","#N/A Invalid Security")) %>% gather(key = "isin",value = "cijena",-datum) %>% mutate(datum=as.Date(datum)) %>% na.omit() %>% arrange(datum) %>% group_by(isin) %>% mutate(povrat=cijena/lag(cijena,6)-1) %>% group_by(isin) %>% summarise(var=quantile(povrat,0.1,na.rm=T))
#temp <- left_join(dionice,temp,by="isin") %>% filter((vrsta0=="Osiguranja" & datum=="2020-03-31") | (vrsta0 %in% c("Mirovinski","Investicijski","Poseban") & datum=="2020-06-30"))
#pom1 <- temp %>% filter(!is.na(var)) %>% group_by(datum,vrsta0) %>% summarise(uzorak=sum(iznos,na.rm = T))
#pom2 <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo" & razina1 %in% c("Equity") & !is.na(iznos)) %>% group_by(datum) %>% summarise(iznos=sum(iznos,na.rm=T)) %>% mutate(vrsta0="Osiguranja")
#pom3 <- nav %>% filter(izvjestaj=="NAV" & razina2=="Dionice") %>% group_by(datum,vrsta0) %>% summarise(iznos=sum(iznos,na.rm = T))
#pom <- inner_join(pom1,rbind(pom2,pom3),by=c("datum","vrsta0")) %>% mutate(udio=uzorak/iznos*100)
#ggplot(pom,aes(x=vrsta0,y=udio,fill=vrsta0)) + geom_col()
#rm(temp,pom1,pom2,pom3)


# Simulacija 

# priprema podataka
pom1 <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo" & razina1=="Equity") %>% select(datum,vrsta1=portfelj,subjekt,razina1,protustrana,iznos,isin,drzava,valuta,vrednovanje) %>% mutate(vrsta0="Osiguranja",vrsta2=NA,vrsta1=case_when(vrsta1=="Non-life [split applicable]"~"Neživot",vrsta1=="Life [split applicable]"~"Život",T~"Ostalo"),razina1="Dionice")
pom2 <- nav %>% filter(izvjestaj=="NAV" & razina2=="Dionice") %>% select(datum,vrsta0,vrsta1,vrsta2=vrsta4,subjekt,razina1=razina3,protustrana,iznos,isin,drzava,valuta,vrednovanje)
# Izračun value at riska - na podacima s bloomberga
temp <- read_excel("dionice_zadnji_datum.xlsx",sheet = "vrijednosti_mjesecni",skip = 5,na = c("#N/A N/A","#N/A Invalid Security")) %>% gather(key = "isin",value = "cijena",-datum) %>% mutate(datum=as.Date(datum)) %>% na.omit() %>% arrange(datum) %>% group_by(isin) %>% mutate(povrat=cijena/lag(cijena,1)-1) %>% group_by(isin) %>% summarise(var=quantile(povrat,kvantil_dionice,na.rm=T))
# spajanje i izračun
dionice <- rbind(pom1,pom2)
pom3 <- dionice %>% filter((vrsta0=="Osiguranja" & datum=="2020-03-31") | (vrsta0 %in% c("Mirovinski","Investicijski","Poseban") & datum=="2020-06-30")) %>% left_join(temp,by="isin") %>% mutate(datum=as.Date("2020-12-31"))
pom4 <- pom3 %>% group_by(subjekt,vrsta1) %>% summarise(var_skupni=weighted.mean(x = var,w = iznos,na.rm=T)) %>% na.omit()
pom4 <- left_join(pom3,pom4,by=c("vrsta1","subjekt")) %>% mutate(var=ifelse(is.na(var),var_skupni,var),iznos=ifelse(is.na(var),iznos,iznos*(1+var))) %>% select(-var,-var_skupni)
dionice <- rbind(dionice,pom4)
pom <- dionice %>% group_by(datum,vrsta0) %>% summarise(iznos=sum(iznos,na.rm=T))
ggplot(pom,aes(x=datum,y=iznos)) + geom_line(size=1.1) + facet_wrap(~vrsta0,scales = "free") + scale_y_continuous(labels = scales::comma)
rm(pom1,pom2,pom3,pom4,temp)
skopiraj(pom)

# 6. Simulacija vrijednosti investicijskih fondova ####

# Popis (svih) investicijskih fondova - input za bloomberg
#pom1 <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo" & razina1=="Investment Funds") %>% select(isin) %>% distinct()
#pom2 <- nav %>% filter(izvjestaj=="NAV" & razina2=="Investicijski fondovi") %>% select(isin) %>% distinct()
#pom <- rbind(pom1,pom2) %>% distinct()
#xlsx::write.xlsx2(pom,file = "popis_ifondova.xlsx",append = T,sheetName = "ukupno")

# Popis investicijskih fondova samo na zadnji datum - input za bloomberg
#pom1 <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo" & razina1=="Investment Funds" & datum=="2020-03-31") %>% select(isin) %>% distinct()
#pom2 <- nav %>% filter(izvjestaj=="NAV" & razina2=="Investicijski fondovi" & datum=="2020-06-30") %>% select(isin) %>% distinct()
#pom <- rbind(pom1,pom2) %>% distinct()
#xlsx::write.xlsx2(pom,file = "popis_ifondova.xlsx",append = T,sheetName = "zadnji_datum")

# udio domaćih fondova
#pom1 <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo" & razina1=="Investment Funds") %>% mutate(rezidentnost=ifelse(drzava=="HR","Domaći","Strani")) %>% group_by(datum,rezidentnost) %>% summarise(iznos=sum(iznos,na.rm=T)) %>% mutate(vrsta0="Osiguranja")
#pom2 <- nav %>% filter(izvjestaj=="NAV" & razina2=="Investicijski fondovi") %>% mutate(rezidentnost=ifelse(drzava=="HR","Domaći","Strani")) %>% group_by(datum,vrsta0,rezidentnost) %>% summarise(iznos=sum(iznos,na.rm=T))
#pom <- rbind(pom1,pom2)
#ggplot(pom,aes(x=datum,y=iznos,fill=rezidentnost)) + geom_col(position="fill") + facet_wrap(~vrsta0,scales="free") + boje_fill
#rm(pom1,pom2)

# pokrivenost simuliranih investicijskih fondova u uzorku
#temp <- read_excel("ifondovi_zadnji_datum.xlsx",sheet = "vrijednosti_mjesecni",skip = 5,na = c("#N/A N/A","#N/A Invalid Security")) %>% gather(key = "isin",value = "cijena",-datum) %>% mutate(datum=as.Date(datum)) %>% na.omit() %>% arrange(datum) %>% group_by(isin) %>% mutate(povrat=cijena/lag(cijena,6)-1) %>% group_by(isin) %>% summarise(var=quantile(povrat,0.1,na.rm=T))
#temp <- left_join(ifondovi,temp,by="isin") %>% filter((vrsta0=="Osiguranja" & datum=="2020-03-31") | (vrsta0 %in% c("Mirovinski","Investicijski","Poseban") & datum=="2020-06-30"))
#pom1 <- temp %>% filter(!is.na(var)) %>% group_by(datum,vrsta0) %>% summarise(uzorak=sum(iznos,na.rm = T))
#pom2 <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo" & razina1 %in% c("Investment Funds") & !is.na(iznos)) %>% group_by(datum) %>% summarise(iznos=sum(iznos,na.rm=T)) %>% mutate(vrsta0="Osiguranja")
#pom3 <- nav %>% filter(izvjestaj=="NAV" & razina2=="Investicijski fondovi") %>% group_by(datum,vrsta0) %>% summarise(iznos=sum(iznos,na.rm = T))
#pom <- inner_join(pom1,rbind(pom2,pom3),by=c("datum","vrsta0")) %>% mutate(udio=uzorak/iznos*100)
#ggplot(pom,aes(x=vrsta0,y=udio,fill=vrsta0)) + geom_col()
#rm(temp,pom1,pom2,pom3)


# Simulacija

# priprema podataka
domaci_fondovi <- (nav_opce %>% filter(vrsta0=="Investicijski" & datum=="2020-06-30") %>% select(isin) %>% distinct() %>% na.omit() %>% filter(isin!="NA"))[,1]
pom1 <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo" & razina1=="Investment Funds") %>% select(datum,vrsta1=portfelj,subjekt,razina1=razina2,protustrana,iznos,isin,drzava,valuta,vrednovanje) %>% mutate(vrsta0="Osiguranja",vrsta2=NA,vrsta1=case_when(vrsta1=="Non-life [split applicable]"~"Neživot",vrsta1=="Life [split applicable]"~"Život",T~"Ostalo"),razina1="Investicijski fondovi")
pom2 <- nav %>% filter(izvjestaj=="NAV" & razina2=="Investicijski fondovi") %>% select(datum,vrsta0,vrsta1,vrsta2=vrsta4,subjekt,razina1=razina3,protustrana,iznos,isin,drzava,valuta,vrednovanje)
# Izračun value at riska - na podacima s bloomberga
temp <- read_excel("ifondovi_zadnji_datum.xlsx",sheet = "vrijednosti_mjesecni",skip = 5,na = c("#N/A N/A","#N/A Invalid Security")) %>% gather(key = "isin",value = "cijena",-datum) %>% mutate(datum=as.Date(datum)) %>% na.omit() %>% arrange(datum) %>% group_by(isin) %>% mutate(povrat=cijena/lag(cijena,1)-1) %>% group_by(isin) %>% summarise(var=quantile(povrat,kvantil_ifondovi,na.rm=T))
# spajanje i izračun
ifondovi <- rbind(pom1,pom2)
pom3 <- ifondovi %>% filter((vrsta0=="Osiguranja" & datum=="2020-03-31") | (vrsta0 %in% c("Mirovinski","Investicijski","Poseban") & datum=="2020-06-30")) %>% left_join(temp,by="isin") %>% mutate(datum=as.Date("2020-12-31"))
# za one fondove za koje nismo našli podatke simulirati ćemo prosječni var od tog društva 
pom4 <- pom3 %>% group_by(subjekt,vrsta1) %>% summarise(var_skupni=weighted.mean(x = var,w = iznos,na.rm=T)) %>% na.omit()
# u ovom koraku stresiramo samo strane fondove preko VaR-a, u drugom koraku ćemo stresirati domaće fondove
pom4 <- left_join(pom3,pom4,by=c("vrsta1","subjekt")) %>% mutate(var=ifelse(is.na(var),var_skupni,var),iznos=ifelse(isin %in% domaci_fondovi,iznos,ifelse(is.na(var),iznos,iznos*(1+var)))) %>% select(-var,-var_skupni)
ifondovi <- rbind(ifondovi,pom4)
pom <- ifondovi %>% group_by(datum,vrsta0) %>% summarise(iznos=sum(iznos,na.rm=T))
ggplot(pom,aes(x=datum,y=iznos)) + geom_line(size=1.1) + facet_wrap(~vrsta0,scales = "free") + scale_y_continuous(labels = scales::comma)
rm(pom1,pom2,pom3,pom4,temp)
skopiraj(pom)

# 7. Cijene nekretnina - osiguranja ####

# nekretnine koje stresiramo
nekretnine <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo" & razina1 %in% c("Property")) %>% select(datum,vrsta1=portfelj,subjekt,razina2,isin,iznos,drzava,valuta,vrednovanje) %>% mutate(vrsta0="Osiguranja",vrsta2=NA,vrsta1=case_when(vrsta1=="Non-life [split applicable]"~"Neživot",vrsta1=="Life [split applicable]"~"Život",T~"Ostalo"),razina1="Ostala imovina")
pom1 <- nekretnine %>% filter(datum=="2020-03-31") %>% mutate(datum=as.Date("2020-12-31"),iznos=ifelse( razina2 %in% c("Property (office and commercial)","Property (residential)","Property (under construction)"),(1+delta_nekret)*iznos,iznos))
nekretnine <- rbind(nekretnine,pom1)
rm(pom1)

# 8. Spajanje svih dijelova zajedno ####
# ostala imovina
pom1 <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo" & !razina1 %in% c("Corporate bonds","Government bonds","Equity","Investment Funds","Property")) %>% select(datum,vrsta1=portfelj,subjekt,razina2,isin,iznos,drzava,valuta,vrednovanje) %>% mutate(vrsta0="Osiguranja",vrsta2=NA,vrsta1=case_when(vrsta1=="Non-life [split applicable]"~"Neživot",vrsta1=="Life [split applicable]"~"Život",T~"Ostalo"),razina1="Ostala imovina")
pom2 <- nav %>% filter(izvjestaj=="NAV" & razina1=="Imovina" & !razina2 %in% c("Dugoročni dužnički vrijednosni papiri","Dionice","Investicijski fondovi")) %>% select(datum,vrsta0,vrsta1,vrsta2=vrsta4,subjekt,razina2,isin,iznos,drzava,valuta,vrednovanje) %>% mutate(razina1="Ostala imovina")
ostatak <- rbind(pom1,pom2)
# vrijednost za kraj 2020. - pretpostavka da se ništa ne mijenja
pom3 <- ostatak %>% filter((vrsta0=="Osiguranja" & datum=="2020-03-31") | (vrsta0 %in% c("Mirovinski","Investicijski","Poseban") & datum=="2020-06-30")) %>% mutate(datum=as.Date("2020-12-31"))
ostatak <- rbind(ostatak,pom3)
# sve zajedno
imovina <- rbind(obveznice %>% select(datum,vrsta0,vrsta1,vrsta2,subjekt,drzava,valuta,vrednovanje,isin,iznos) %>% mutate(razina1="Obveznice"),dionice %>% select(datum,vrsta0,vrsta1,vrsta2,subjekt,drzava,valuta,vrednovanje,isin,iznos) %>% mutate(razina1="Dionice"), ifondovi %>% select(datum,vrsta0,vrsta1,vrsta2,subjekt,drzava,valuta,vrednovanje,isin,iznos) %>% mutate(razina1="Investicijski fondovi"),nekretnine %>% select(datum,vrsta0,vrsta1,vrsta2,subjekt,drzava,valuta,vrednovanje,isin,iznos) %>% mutate(razina1="Nekretnine"),ostatak %>% select(-razina2))
rm(pom1,pom2,pom3)

# 9. Vrijednost domaćih investicijskih fondova - 2nd round efekti (look through approach) ####

# 1. Runda umanjenja

# izračun pada cijene udjela po fondu
pom11 <- imovina %>% filter(vrsta0=="Investicijski" & (datum=="2020-06-30" | datum=="2020-12-31")) %>% group_by(datum,subjekt) %>% summarise(iznos=sum(iznos,na.rm=T))
pom12 <- nav_opce %>% select(subjekt,isin) %>% distinct()
pom1 <- left_join(pom11,pom12,by=c("subjekt")) %>% na.omit() %>% filter(isin!="NA") %>% spread(datum,iznos) %>% mutate(delta_cj_udjela=`2020-12-31`/`2020-06-30`-1) %>% select(isin,delta_cj_udjela)
# umanjenje vrijednosti cijene udjela u imovini
imovina <- imovina %>% left_join(pom1,by="isin") %>% mutate(iznos=ifelse(is.na(delta_cj_udjela),iznos,iznos*(1+delta_cj_udjela))) %>% select(-delta_cj_udjela)

# 2. Runda umanjenja

# izračun pada cijene udjela po fondu
pom11 <- imovina %>% filter(vrsta0=="Investicijski" & (datum=="2020-06-30" | datum=="2020-12-31")) %>% group_by(datum,subjekt) %>% summarise(iznos=sum(iznos,na.rm=T))
pom12 <- nav_opce %>% select(subjekt,isin) %>% distinct()
pom1 <- left_join(pom11,pom12,by=c("subjekt")) %>% na.omit() %>% filter(isin!="NA") %>% spread(datum,iznos) %>% mutate(delta_cj_udjela=`2020-12-31`/`2020-06-30`-1) %>% select(isin,delta_cj_udjela)
# umanjenje vrijednosti cijene udjela u imovini
imovina <- imovina %>% left_join(pom1,by="isin") %>% mutate(iznos=ifelse(is.na(delta_cj_udjela),iznos,iznos*(1+delta_cj_udjela))) %>% select(-delta_cj_udjela)

# 3. Runda umanjenja

# izračun pada cijene udjela po fondu
pom11 <- imovina %>% filter(vrsta0=="Investicijski" & (datum=="2020-06-30" | datum=="2020-12-31")) %>% group_by(datum,subjekt) %>% summarise(iznos=sum(iznos,na.rm=T))
pom12 <- nav_opce %>% select(subjekt,isin) %>% distinct()
pom1 <- left_join(pom11,pom12,by=c("subjekt")) %>% na.omit() %>% filter(isin!="NA") %>% spread(datum,iznos) %>% mutate(delta_cj_udjela=`2020-12-31`/`2020-06-30`-1) %>% select(isin,delta_cj_udjela)
# umanjenje vrijednosti cijene udjela u imovini
imovina <- imovina %>% left_join(pom1,by="isin") %>% mutate(iznos=ifelse(is.na(delta_cj_udjela),iznos,iznos*(1+delta_cj_udjela))) %>% select(-delta_cj_udjela)

# brisanje pomoćnih dataframeova
rm(pom11,pom12,pom1,domaci_fondovi)

# 10. Dizanje ulaganja od osiguranja na razinu imovine ####
pom1 <- (imovina %>% filter(vrsta0=="Osiguranja" & datum=="2020-12-31") %>% summarise(iznos=sum(iznos,na.rm=T)))[1,1] # iznos ulaganja
pom2 <- pom1/0.88 # iznos imovine
pom3 <- pom2 - pom1 # iznos koji treba dodijeliti ostaloj imovini
pom4 <- (imovina %>% filter(vrsta0=="Osiguranja" & datum=="2020-12-31" & razina1=="Ostala imovina") %>% summarise(iznos=sum(iznos,na.rm=T)))[1,1]
ost_imo_os_faktor <- pom3/pom4+1
imovina <- imovina %>% mutate(iznos=ifelse(vrsta0=="Osiguranja" & razina1=="Ostala imovina",iznos*ost_imo_os_faktor,iznos))
rm(pom1,pom2,pom3,pom4,ost_imo_os_faktor)

# 11. Dizanje vrijednosti imovine mirovinskih fondova za pozitivne neto doprinose ####
pom1 <- imovina %>% filter(vrsta0=="Mirovinski" & datum=="2020-12-31") %>% group_by(vrsta1) %>% summarise(iznos=sum(iznos,na.rm=T)) %>% left_join(data.frame(vrsta1=c("Dobrovoljan","Obavezan"),doprinos=c(doprinosi_dmf,doprinosi_omf)),by="vrsta1") %>% mutate(mf_faktor=doprinos/iznos) %>% select(vrsta1,mf_faktor)
neto_uplate <- imovina %>% filter(vrsta0=="Mirovinski" & datum=="2020-12-31") %>% group_by(datum,vrsta0,vrsta1,vrsta2,subjekt) %>% summarise(iznos=sum(iznos,na.rm=T)) %>% left_join(pom1,by="vrsta1") %>% mutate(iznos=iznos*mf_faktor) %>% select(-mf_faktor) %>% mutate(drzava=NA,valuta=NA,vrednovanje=NA,isin=NA,razina1="Neto uplate")
# neto uplate isto treba umanjiti za postotak za koji smo smanjili ostatak imovine
pom2 <- imovina %>% filter(vrsta0=="Mirovinski" & (datum=="2020-06-30" | datum=="2020-12-31")) %>% group_by(datum,subjekt) %>% summarise(iznos=sum(iznos,na.rm=T)) %>% spread(datum,iznos) %>% mutate(umanjenje=`2020-12-31`/`2020-06-30`) %>% select(subjekt,umanjenje)
neto_uplate <- left_join(neto_uplate,pom2,by="subjekt") %>% mutate(iznos=iznos*umanjenje) %>% select(-umanjenje)
rm(pom1,pom2)

# 12. Tečaj ####

# Dohvat tečaja sa EUR/HRK (s ECB-a)
pom1 <- get_data("EXR.Q.HRK+USD.EUR.SP00.E") %>% select(obstime,currency,tecaj=obsvalue) %>% mutate(datum = make_date(ifelse(substr(obstime,7,7)=="4",as.numeric(substr(obstime,1,4))+1,as.numeric(substr(obstime,1,4))), ifelse(substr(obstime,7,7)=="4",1,as.numeric(substr(obstime,7,7))*3+1), 1)-1) %>% select(-obstime) %>% spread(currency,tecaj)
# dohvaćanje VaR vrijednosti deprecijacije tečaja
fx_faktor <- pom1 %>% arrange(datum) %>% mutate(dtecaj=HRK/lag(HRK,2)-1) %>% summarise(fx_var=quantile(dtecaj,probs = kvantil_tecaj,na.rm = T)) %>% as.numeric()
# izračun
tecaj <- imovina %>% filter(datum=="2020-12-31") %>% group_by(datum,vrsta0,vrsta1,vrsta2,subjekt,valuta) %>% summarise(iznos=sum(iznos,na.rm=T)) %>% mutate(iznos=ifelse(!valuta %in% c("HRK","Nepoznata domena","N/A"),iznos*fx_faktor,0)) %>% mutate(drzava=NA,vrednovanje=NA,isin=NA,razina1="Tečajne promjene")
rm(pom1,fx_faktor)

# 13. Konačno spajanje imovine ####
imovina <- rbind(imovina,neto_uplate,tecaj)
save(imovina,file="imovina_sim.Rda")


pom <- imovina %>% group_by(datum,vrsta1) %>% summarise(iznos=sum(iznos,na.rm=T)) %>% spread(vrsta1,iznos)
skopiraj(pom)

# dekompozicija promjene
pom0 <- imovina %>% filter((vrsta0=="Osiguranja" & datum=="2020-03-31") | (vrsta0!="Osiguranja" & datum=="2020-06-30")) %>% filter(vrsta0!="Poseban" & vrsta1!="AIF") %>% group_by(vrsta0) %>% summarise(baza=sum(iznos,na.rm=T))
pom <- imovina %>% filter((vrsta0=="Osiguranja" & datum=="2020-03-31") | (vrsta0!="Osiguranja" & datum=="2020-06-30") | datum=="2020-12-31") %>% filter(vrsta0!="Poseban" & vrsta1!="AIF") %>% group_by(datum,vrsta0,razina1) %>% summarise(iznos=sum(iznos,na.rm=T)) %>% arrange(datum) %>% group_by(vrsta0,razina1) %>% mutate(iznos=ifelse(!razina1 %in% c("Tečajne promjene","Neto uplate"),iznos-lag(iznos,1),iznos)) %>% na.omit() %>% left_join(pom0,by="vrsta0") %>% mutate(iznos=iznos/baza)
pom1 <- pom %>% group_by(vrsta0) %>% summarise(iznos=sum(iznos,na.rm=T))
ggplot(pom,aes(x=vrsta0,y=iznos,fill=razina1)) + geom_col(alpha=0.9) + facet_grid(~vrsta0,scales = "free_x") + boje_fill + scale_y_continuous(labels = percent) + geom_hline(yintercept = 0,col="red",linetype=2,size=1.1) + geom_point(data = pom1,inherit.aes = F,aes(x=vrsta0,y=iznos),size=4) + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) + labs(x="",y="",caption="Izvor: Hanfa",subtitle = "Dekompozicija relativne promjene imovine društava, u %",title="Slika XX. Bla")
rm(pom0,pom1)

# distribucija po društvu
pom <- imovina %>% filter((vrsta0=="Osiguranja" & datum=="2020-03-31") | (vrsta0!="Osiguranja" & datum=="2020-06-30") | datum=="2020-12-31") %>% filter(vrsta0!="Poseban" & vrsta1!="AIF") %>% group_by(datum,vrsta0,subjekt) %>% summarise(iznos=sum(iznos,na.rm=T)) %>% arrange(datum) %>% group_by(vrsta0,subjekt) %>% mutate(delta=iznos/lag(iznos,na.rm=T)-1) %>% na.omit()
ggplot(pom %>% filter(subjekt!="Triglav Emerging Bond (HRNFDAUEMBA7)"),aes(x=1,y=delta,col=vrsta0)) + geom_jitter(aes(size=iznos),alpha=0.6) + facet_wrap(~vrsta0,scales = "free_x") + boje_col  + scale_y_continuous(labels = percent) + theme(legend.position = "none",axis.text.x=element_blank(),axis.ticks.x=element_blank()) + labs(x="",y="",caption="Napomena: Veličina točke označuje simuliranu veličinu fonda, odnosno društva za osiguranje na kraju 2020.\nIzvor: Hanfa",subtitle = "Relativna promjena vrijednosti imovine, u %", title = "Slika XX. Izražena divergencija rezultata unutar pojedinih industrija") + scale_size_continuous(range = c(2, 10))
