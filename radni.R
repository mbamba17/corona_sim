#library(mFilter)
library(tidyverse)
library(scales)
library(ggthemes)
library(extrafont)
library(eurostat)
library(lubridate)
library(readxl)
library(xlsx)

# Funkcija za kopiranje u excel ####
skopiraj <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

# Datum i regija ####
reg = data.frame(ref_area=c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE","GB","U2","I8"),country=c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovak Republic","Slovenia","Spain","Sweden","Great Britain","EA changing comp.","Euro Area 19"),regija=c("Other EU","Other EU","CEE","HR","Other EU","CEE","Other EU","CEE","Other EU","Other EU","Other EU","Other EU","CEE","Other EU","Other EU","CEE","CEE","Other EU","Other EU","Other EU","CEE","Other EU","CEE","CEE","CEE","Other EU","Other EU","Other EU","EA changing comp.","Euro Area 19"))


pom <- get_eurostat(id="prc_hicp_manr") %>% filter(coicop=="CP00" & unit=="RCH_A") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% filter(datum>"2005-12-31" & geo %in% reg$ref_area)
ggplot(pom,aes(x=datum,y=values,col=geo)) + geom_line()

# 1. Procjena stope nezaposlenosti ####
temp <- read_excel(path = "radni.xlsx",sheet="nezaposlenost")
#v1
mdl <- lm(dunemp~dbdp_r,data = temp)
summary(mdl)
#v2
mdl <- lm(dunemp~dbdp_r+lag(unemp,1),data = temp)
summary(mdl)

# 2. Procjena kretanja bruto plaća ####
wage <- read_excel(path = "place.xlsx",sheet="izvoz_r",range="A1:F89")
#v1
mdl <- lm(dwage~infl+unemp+dprod,data = wage)
summary(mdl)
plot(mdl)
pom <- data.frame(mdl$model,reziduali=mdl$residuals) %>% mutate(dwage_proc=dwage-reziduali)
skopiraj(pom)

# 3. Podaci s porezne ####

pu_mjesecni <- read_excel(path="porezna_podaci.xlsx",sheet="mjesecno") %>% mutate(datum=ceiling_date(make_date(godina,mjesec,1),unit = "month")-1) %>% mutate(kvartal=ceiling_date(datum,unit = "quarter")-1)
pu_tjedni <- read_excel(path="porezna_podaci.xlsx",sheet="tjedno")
# agregatni trend po mjesecima
pom <- pu_mjesecni %>% group_by(godina,mjesec) %>% filter(!(godina==2020 & mjesec==4)) %>% summarise(iznos=sum(iznos,na.rm = T),broj=sum(broj,na.rm=T)) %>% gather(key = "vrsta_iznosa",value = "iznos",-godina,-mjesec)
ggplot(pom,aes(x=mjesec,y=iznos,col=factor(godina))) + geom_line() + facet_wrap(~vrsta_iznosa,scales = "free")+ boje_col + scale_y_continuous(labels = scales::comma) + expand_limits(y=0) + theme(legend.position = "top")
# trend po mjesecima i po djelatnostima
pom <- pu_mjesecni %>% group_by(godina,mjesec,naziv1) %>% filter(!(godina==2020 & mjesec==4)) %>% summarise(iznos=sum(iznos,na.rm = T)) %>% arrange(godina) %>% group_by(mjesec,naziv1) %>% mutate(delta=iznos/lag(iznos,1)-1) %>% na.omit()
ggplot(pom,aes(x=mjesec,y=delta,fill=factor(godina))) + geom_col(position="dodge") + facet_wrap(~naziv1,scales = "free")+ boje_fill + scale_y_continuous(labels = scales::percent) + theme(legend.position = "top") + scale_x_continuous(breaks = 1:12)

pom <- pu_mjesecni %>% group_by(kvartal,razina1) %>% summarise(iznos=sum(iznos,na.rm = T)) %>% spread(kvartal,iznos)
skopiraj(pom)

# kretanje po tjednima
pom <- pu_tjedni %>% group_by(mj_tj,godina) %>% filter(!(godina==2020 & mjesec==4)) %>% summarise(iznos=sum(iznos,na.rm = T),broj=sum(broj,na.rm=T)) %>% gather(key = "vrsta_iznosa",value = "iznos",-godina,-mj_tj)
ggplot(pom,aes(x=mj_tj,y=iznos,col=factor(godina),group=godina)) + geom_line() + facet_wrap(~vrsta_iznosa,scales = "free")+ boje_col + scale_y_continuous(labels = scales::comma) + expand_limits(y=0) + theme(legend.position = "top")

pom <- pu_tjedni %>% group_by(godina,mj_tj,razina1) %>% summarise(iznos=sum(iznos,na.rm = T)) %>% spread(mj_tj,iznos)
skopiraj(pom)

pom <- pu_mjesecni %>% group_by(godina,razina1) %>% summarise(iznos=sum(iznos,na.rm = T)) %>% spread(godina,iznos)
skopiraj(pom)

# 4. Model za kredite ####

kred <- read_excel(path = "radni.xlsx",sheet = "krediti",range="A1:E81") %>% mutate(datum=as.Date(datum)) %>% select(-unemp_gap)
kred$unemp_gap = (hpfilter(kred$unemp,freq=25000))$cycle
kred <- kred %>% filter(datum>"2007-12-31")
# sa lagom i bdp-om
kred_mdl <- lm(dkred~lag(dkred,4)+dbdp_r,data = kred)
summary(kred_mdl)
# sa gapom nezaposlenosti i bdp-om
kred_mdl <- lm(dkred~unemp_gap+dbdp_r,data = kred)
summary(kred_mdl)


# 5. Model za inflaciju ####
infl <- read_excel(path = "radni.xlsx",sheet = "inflacija",range = "A2:L82") %>% mutate(datum=as.Date(datum))
#pom <- get_eurostat("prc_hicp_manr") %>% filter(coicop=="CP00" & geo=="EU") %>% mutate(datum = ceiling_date(time,"month")-1) %>% select(datum,infl_eu=values)
#infl <- left_join(infl,pom,by="datum")
infl$unemp_s1_gap = (hpfilter(infl$unemp_s1,freq=25000))$cycle
infl$unemp_s2_gap = (hpfilter(infl$unemp_s2,freq=25000))$cycle
infl$unemp_s3_gap = (hpfilter(infl$unemp_s3,freq=25000))$cycle
# inflacija
infl_mdl <- lm(infl~unemp_gap+infl_eu,data = infl)
summary(infl_mdl)
# core inflacija
infl_mdl <- lm(infl_core~unemp_gap+infl_eu,data = infl)
summary(infl_mdl)

skopiraj(infl)

# 6. Model za tečaj ####
fx <- read_excel(path = "radni.xlsx",sheet = "tecaj",range = "A1:f81") %>% mutate(datum=as.Date(datum)) %>% na.omit()
# model samo za tekućim računom
fx_mdl <- lm(tecaj~reserv+curacc,data = fx)
summary(fx_mdl)
# model samo za ukupnom bilancom (bez rezervi)
fx_mdl <- lm(tecaj~reserv+bop,data = fx)
summary(fx_mdl)

# model samo za tekućim računom
fx_mdl <- lm(dtecaj~reserv+curacc,data = fx)
summary(fx_mdl)
# model samo za ukupnom bilancom (bez rezervi)
fx_mdl <- lm(dtecaj~reserv+bop,data = fx)
summary(fx_mdl)

# 7. Model za kamatne stope ####
kstope <- read_excel(path = "radni.xlsx",sheet = "kstope",range = "A1:f73") %>% mutate(datum=as.Date(datum)) %>% na.omit()
kstope$dkred_gap = (hpfilter(kstope$dkred,freq=25000))$cycle
# kratkoročne kstope
ks_mdl <- lm(kks~lag(kks,1)+prinos+euribor,data = kstope)
summary(ks_mdl)
# dugoročne kstope
ks_mdl <- lm(dks~lag(kks,1)+lag(dks,1),data = kstope)
summary(ks_mdl)

# 8. HZZ potpore - podaci ####
library(rjson)
library(jsonlite)
base_url = "https://mjera-orm.hzz.hr/korisnici-potpore/ozujak-2020/json/"
hzz_podaci <- fromJSON(base_url)
hzz_podaci %>% summarise(SupportPaidAmount=sum(SupportPaidAmount),SupportedEmployeeNumber=sum(SupportedEmployeeNumber))

# 9. Traženje korijena za izračun implicitnog prinosa do dospijeća ####
library(pracma)
load("Z:/DSR/DWH/NAV.Rda")
pom <- nav %>% filter(vrsta1 %in% c("UCITS","Obavezan","Dobrovoljan") & razina2=="Dugoročni dužnički vrijednosni papiri" & datum=="2020-04-30") %>% select(subjekt,razina3,drzava,valuta,protustrana,isin,vrednovanje,dospijece,kamata,nominala_u_valuti,kamata_u_valuti,jed_cijena_u_valuti,tecaj,iznos)
write.xlsx2(pom,file="bla.xlsx")
pom1 <- pom[1,]
pom1$kamata <- 0.05875
pom1$preostalo_dospijece <- as.numeric((as.Date(pom1$dospijece) - as.Date("2020-04-30"))/365)
#pom1$cijena <- pom1$nominala_u_valuti*pom1$jed_cijena_u_valuti*pom1$tecaj
pom1 <- pom1 %>% mutate(cijena=nominala_u_valuti*jed_cijena_u_valuti*tecaj)
bisect(function(r) (pom1$nominala_u_valuti*pom1$tecaj)/((1+r/2)^(2*pom1$preostalo_dospijece)) + ((pom1$kamata*pom1$nominala_u_valuti*pom1$tecaj/2)/(r/2))*(1-(1/((1+r/2)^(2*pom1$preostalo_dospijece)))) - pom1$cijena, 0.000001, 1)

# Dionice
pom1 <- imovina_s2 %>% filter(modul=="Quarterly Solvency II reporting Solo" & razina1=="Equity") %>% select(datum,vrsta1=portfelj,subjekt,razina1,protustrana,iznos,isin,drzava,valuta,vrednovanje) %>% mutate(vrsta0="Osiguranja",vrsta2=NA,vrsta1=case_when(vrsta1=="Non-life [split applicable]"~"Neživot",vrsta1=="Life [split applicable]"~"Život",T~"Ostalo"),razina1="Dionice")
pom2 <- nav %>% filter(izvjestaj=="NAV" & razina2=="Dionice") %>% select(datum,vrsta0,vrsta1,vrsta2=vrsta4,subjekt,razina1=razina3,protustrana,iznos,isin,drzava,valuta,vrednovanje)
# Izračun value at riska - na podacima s bloomberga
temp <- read_excel("dionice_zadnji_datum.xlsx",sheet = "vrijednosti_mjesecni",skip = 5,na = c("#N/A N/A","#N/A Invalid Security")) %>% gather(key = "isin",value = "cijena",-datum) %>% mutate(datum=as.Date(datum)) %>% na.omit() %>% arrange(datum) %>% group_by(isin) %>% mutate(povrat=cijena/lag(cijena,1)-1) %>% group_by(isin) %>% summarise(var=quantile(povrat,kvantil_dionice,na.rm=T))
# spajanje i izračun
dionice <- rbind(pom1,pom2)
pom3 <- dionice %>% filter((vrsta0=="Osiguranja" & datum=="2020-03-31") | (vrsta0 %in% c("Mirovinski","Investicijski","Poseban") & datum=="2020-06-30")) %>% left_join(temp,by="isin") %>% mutate(datum=as.Date("2020-12-31"))
pom4 <- pom3 %>% group_by(subjekt,vrsta1) %>% summarise(var_skupni=weighted.mean(x = var,w = iznos,na.rm=T)) %>% na.omit() %>% mutate()
pom4 <- left_join(pom3,pom4,by=c("vrsta1","subjekt")) %>% mutate(var=ifelse(is.na(var),var_skupni,var),iznos=ifelse(is.na(var),iznos,iznos*(1+var)))
# crtanje
pom <- pom4 %>% group_by(vrsta0,drzava,var) %>% summarise(iznos=sum(iznos,na.rm=T)) %>% distinct() %>% mutate(drzava=ifelse(drzava %in% c("HR","US"),drzava,ifelse(drzava %in% reg$geo,"EU","OST")))
ggplot(pom,aes(x=1,y=var,col=drzava)) + geom_jitter(alpha=0.6,aes(size=iznos)) + facet_wrap(~vrsta0,scales = "free_x") + boje_col  + scale_y_continuous(labels = percent) + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) + labs(x="",y="",caption="Napomena: Veličina točke označuje simuliranu veličinu fonda, odnosno društva za osiguranje na kraju 2020.\nIzvor: Hanfa",subtitle = "Relativna promjena vrijednosti imovine, u %", title = "Slika XX. Izražena divergencija rezultata unutar pojedinih industrija") + scale_size_continuous(range = c(2, 10))
rm(pom1,pom2,pom3,pom4)

# Usporedba investicijskih i mirovinskih fondova u ožujku 2020.
pom <- nav_opce %>% filter(izvjestaj=="NAV" & vrsta0!="Poseban" & (datum=="2020-01-31" | datum=="2020-03-31")) %>% select(datum,vrsta0,subjekt,cj_udjela_u_valuti,nav) %>% arrange(datum) %>% group_by(vrsta0,subjekt) %>% mutate(prinos=cj_udjela_u_valuti/lag(cj_udjela_u_valuti)-1) %>% na.omit()
ggplot(pom %>% filter(subjekt!="InterCapital Short Term Bond (HRVBINUVBCA6)"),aes(x=1,y=prinos,col=vrsta0)) + geom_jitter(alpha=0.6,aes(size=nav)) + facet_wrap(~vrsta0,scales = "free_x") + boje_col  + scale_y_continuous(labels = percent) + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

pom <- nav_opce %>% filter(izvjestaj=="NAV" & vrsta0!="Poseban" & (datum=="2020-01-31" | datum=="2020-03-31")) %>% select(datum,vrsta0,vrsta1,vrsta2,vrsta4,subjekt,cj_udjela_u_valuti,nav) %>% arrange(datum) %>% group_by(vrsta0,subjekt) %>% mutate(prinos=cj_udjela_u_valuti/lag(cj_udjela_u_valuti)-1) %>% group_by(vrsta0,vrsta1,vrsta2,vrsta4) %>% summarise(prinos=weighted.mean(prinos,w = nav,na.rm = T))
