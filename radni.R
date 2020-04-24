library(mFilter)
library(tidyverse)
library(scales)
library(ggthemes)
library(extrafont)
library(eurostat)
library(lubridate)
library(readxl)

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
