library(ggalluvial)
library(tidyverse)
library(scales)
library(ggthemes)
library(extrafont)
library(eurostat)
library(lubridate)
library(ecb)
library(readxl)

# Funkcija za kopiranje u excel ####
skopiraj <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

gtema <- theme_minimal() + theme(panel.background = element_rect(fill="#e7eaf6",linetype = 0),plot.background = element_rect(fill="#e7eaf6",linetype = 0),legend.box.background = element_rect(fill="#e7eaf6",linetype = 0),text = element_text(colour = "#000000"),plot.caption = element_text(hjust = 0),legend.position = "top",legend.title = element_blank(),panel.border = element_blank(),axis.line = element_blank(),panel.grid.major = element_line(size = 0.5, linetype = "dotted",colour = "#233142"))
theme_set(gtema)

# paleta boja
boje_fill <- scale_fill_manual(values = c("#155e63","#e84545","#25a55f","#ffc93c","#9b5d73","#ff7c38","#7e6752","#679186","#2e99b0","#01d28e","#cd8d7b","#bbbbbb","#f7be16","#b5525c","#4f81c7","#ff8a5c","#32ff6a","#393e46","#df0054","#f69314"))
boje_col <- scale_color_manual(values = c("#155e63","#e84545","#25a55f","#ffc93c","#9b5d73","#ff7c38","#7e6752","#679186","#2e99b0","#01d28e","#cd8d7b","#bbbbbb","#f7be16","#b5525c","#4f81c7","#ff8a5c","#32ff6a","#393e46","#df0054","#f69314"))

# Member States of the European Union
reg = data.frame(geo=c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE","GB"),country=c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Rep.","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom"),regija=c("Ostale zemlje EU","Ostale zemlje EU","Zemlje SIE","HR","Ostale zemlje EU","Zemlje SIE","Ostale zemlje EU","Zemlje SIE","Ostale zemlje EU","Ostale zemlje EU","Ostale zemlje EU","Ostale zemlje EU","Zemlje SIE","Ostale zemlje EU","Ostale zemlje EU","Zemlje SIE","Zemlje SIE","Ostale zemlje EU","Ostale zemlje EU","Ostale zemlje EU","Zemlje SIE","Ostale zemlje EU","Zemlje SIE","Zemlje SIE","Zemlje SIE","Ostale zemlje EU","Ostale zemlje EU","Ostale zemlje EU"))

# Slika 0. COVID-19 podaci ####
library(rjson)
library(jsonlite)
covid <- fromJSON("https://api.covid19api.com/all")
covid <- covid %>% mutate(datum=as.Date(Date))
save(covid,file="covid.Rda")
pom1 <- covid %>% filter(CountryCode %in% c("HR","CN","US","IT","ES","DE","UK","SE","RU")) %>% select(datum,geo=CountryCode,Confirmed,Deaths,Recovered,Active) %>% group_by(datum,geo) %>% summarise(Slučajevi=sum(Confirmed,na.rm=T),Preminuli=sum(Deaths,na.rm=T),Oporavljeni=sum(Recovered,na.rm=T),Aktivni=sum(Active,na.rm=T))
pom2 <- data.frame(geo=c("HR","CN","US","IT","ES","DE","UK","SE","RU"),population=c(4.089,1392,327.167,60.431,46.724,82.928,66.489,10.183,144.478))
pom <- left_join(pom1,pom2,by="geo") %>% mutate(`Slučajevi`=`Slučajevi`/population,Preminuli=Preminuli/population,Oporavljeni=Oporavljeni/population,Aktivni=Aktivni/population) %>% select(-population)%>% gather(value = "broj",key = "varijabla",-datum,-geo) %>% filter(datum>="2020-03-01")
ggplot(pom,aes(x=datum,y=broj,col=geo)) + geom_line(size=1.2) + facet_wrap(~varijabla,scales="free") + boje_col + labs(x="",y="Broj slučajeva",title="Zdravstvena kriza u većini zemalja prošla vrhunac",caption="Izvor: John Hopkins CSSE")
rm(pom1,pom2,covid)

# Slika 01. Lockdown tracker ####
library(rio)
pom <- rio::import(file="https://covid19-lockdown-tracker.netlify.com/lockdown_dates.csv")
pom <- pom1 %>% filter((Country %in% reg$country | Country %in% c("Czech Rep.")) & Level=="National") %>% select(geo=Country,start=`Start date`,end=`End date`) %>% mutate(start=as.Date(start),end=as.Date(end))
ggplot(pom) + geom_segment(aes(x=start,xend=end,y=geo,yend=geo),size=1.05,col="#155e63") + geom_point(size=3,aes(x=start,y=geo),col="#E84545") + geom_point(size=3,aes(x=end,y=geo),col="#01D28E") + labs(x="",y="",title="Epidemijske mjere u većini zemalja EU bliže se kraju",caption = "Izvor: Aura Vision") + geom_vline(xintercept = as.Date("2020-05-15"),col="#ffc93c")


# Slika 1. Struktura BDP-a proizvodna metoda ####

# doprinosi rastu
pom1 <- get_eurostat(id="namq_10_a10") %>% filter(s_adj=="SCA" & na_item=="B1G" & unit=="CLV10_MEUR") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,geo,sektor=nace_r2,values) %>% filter(!sektor %in% c("TOTAL","C")) %>% mutate(sektor=case_when(sektor=="A"~"Poljoprivreda",sektor=="B-E"~"Industrija",sektor=="F"~"Građevinarstvo",sektor=="G-I"~"Trgovina, prijevoz i smještaj",sektor=="J"~"Ostale sluge",sektor=="K"~"Financije i promet nekretninama",sektor=="L"~"Financije i promet nekretninama",sektor=="M_N"~"Ostale usluge",sektor=="O-Q"~"Javna uprava",sektor=="R-U"~"Ostale usluge")) %>% inner_join(reg,by="geo") %>% group_by(datum,regija,sektor) %>% summarise(values=sum(values,na.rm=T)) %>% group_by(regija,sektor) %>% mutate(bdv_y=values+lag(values,1)+lag(values,2)+lag(values,3)) %>% select(datum,regija,sektor,bdv_y)
pom2 <- pom1 %>% group_by(datum,regija) %>% summarise(bdv_total=sum(bdv_y,na.rm=T))
pom <- inner_join(pom1,pom2,by=c("datum","regija")) %>% mutate(udio=bdv_y/bdv_total) %>% mutate(doprinos=(bdv_y/lag(bdv_y,4)-1)*100*lag(udio,4))%>% filter(datum>="2001-12-31" & datum<="2019-12-31")
pom2 <- pom2 %>% group_by(regija) %>% mutate(dbdv=(bdv_total/lag(bdv_total,4)-1)*100) %>% filter(datum>="2001-12-31" & datum<="2019-12-31")
ggplot(pom,aes(x=datum,y=doprinos)) + geom_col(alpha=0.8,aes(fill=sektor))  + geom_line(data = pom2,aes(x=datum,y=dbdv),size=1.5) + facet_wrap(~regija) + boje_fill + labs(x="",y="Doprinosi realnom rastu BDV-a (u %)",title = "Najveći dio rasta BDV generirju trgovina i uslužne djelatnosti",subtitle = "Doprinosi pojedinih djelatnosti realnom rastu bruto dodane vrijednosti",caption = "Izvor: Eurostat") + scale_y_continuous(breaks = -7:7)

# Slika 2. Shematski prikaz simulacije ####
# 1.1. YOY stopa rasta BDP-a
pom <- get_eurostat(id="namq_10_gdp") %>% filter(geo=="HR" & na_item=="B1GQ" & s_adj=="NSA" & unit=="CLV10_MNAC") %>% select(time,bdp=values) %>% arrange(time) %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% as.data.frame() %>% mutate(bdp_y=bdp+lag(bdp,1)+lag(bdp,2)+lag(bdp,3),dbdp=(bdp_y/lag(bdp_y,4)-1)*100) %>% mutate(dbdp_m=(dbdp+lag(dbdp,1)+lag(dbdp,2)+lag(dbdp,3)+lag(dbdp,4)+lag(dbdp,5)+lag(dbdp,6)+lag(dbdp,7))/8) %>% select(datum,dbdp_m) %>% na.omit() %>% filter(datum>="2007-12-31")
pom1 <- data.frame(datum=rep(c("2019-12-31","2020-03-31","2020-06-30","2020-09-30","2020-12-31"),3),scenarij=c(rep("Scenarij1",5),rep("Scenarij2",5),rep("Scenarij3",5)),dbdp_m=c(2.8958114,-1,-2,-4,-5,2.8958114,-1.5,-3,-6,-7,2.8958114,-2,-4,-7,-9),raspon=rep(c(0,0.5,1,2,3),3)) %>% mutate(datum=as.Date(datum)) %>% mutate(min_int=dbdp_m-raspon,max_int=dbdp_m+raspon)
ggplot(pom,aes(x=datum,y=dbdp_m)) + geom_line(size=2) + geom_line(data = pom1,size=2,aes(x=datum,y=dbdp_m,col=scenarij)) + geom_ribbon(data = pom1,aes(ymin=min_int,ymax=max_int,fill=scenarij),alpha=0.2)+ labs(x="",y="Godišnja stopa rasta realnog BDP-a (%)",title="Projekcija efekata trenutne krize sa sobom nose veliku maržu pogreške",subtitle="Shematski prikaz pouzdanosti projekcije utjecaja COVID-19 krize na gospodarstvo",caption="Napomena: Kretanje BDP-a u različitim scenarijima prikazano je isključivo u ilustrativne svrhe.\nIzvor: Hanfa") + boje_col + boje_fill

# Slika 3. U dobrim vremenima svi konvergiraju, a u lošim divegiraju ####
pom <- get_eurostat(id="namq_10_gdp") %>% filter(na_item=="B1GQ" & s_adj=="NSA" & unit=="CLV10_MNAC" & !geo %in% c("EA","EA12","EA19","EU15","EU27_2020","EU28")) %>% select(time,geo,bdp=values) %>% arrange(time) %>% as.data.frame() %>% group_by(geo) %>% mutate(bdp_y = (bdp + lag(bdp,1)+ lag(bdp,2) + lag(bdp,3)),dbdp=(bdp_y/lag(bdp_y,4)-1)*100) %>% mutate(datum = ceiling_date(time,"month")-1) %>% select(datum,geo,dbdp) %>% left_join(reg,by="geo") %>% na.omit() %>% group_by(datum) %>% summarise(pc_10=quantile(dbdp,probs = 0.1,na.rm = T),pc_90=quantile(dbdp,probs = 0.9,na.rm = T),dbdp=mean(dbdp,na.rm=T)) %>% mutate(iqr=pc_90-pc_10) %>% ungroup() %>% filter(datum>"1990-12-31")
ggplot(pom,aes(x=datum,y=dbdp)) + geom_ribbon(aes(ymin=pc_10,ymax=pc_90), fill = "#155e63",alpha=0.4) + geom_ribbon(aes(ymin=-20,ymax=iqr-20), fill = "#e84545",alpha=0.4) + geom_line(size=1.5) + labs(x="",y="Godišnja stopa rasta (%)",title="U kriznim razdobljima raste divergencija",subtitle="Raspon gospodarskog rasta 10% najboljih i najlošijih europskih zemalja naglo raste u kriznim razdobljima",caption = "Izvor: Eurostat") + scale_x_date(date_labels = "%Y.",breaks = "5 years") + boje_col + scale_y_continuous(limits = c(-20,15),breaks = seq(-20,15,by=5),sec.axis = sec_axis(~ . + 20, name = derive()))

# Slika 4. Kratkoročni indikatori poslovne aktivnosti - supply strana ####
# Promet trgovine
pom <- get_eurostat("sts_trtu_m") %>% filter(nace_r2=="G47" & s_adj=="SCA" & indic_bt=="TOVV" & unit=="I15") %>% mutate(datum = ceiling_date(time,"month")-1) %>% left_join(reg,by="geo") %>% mutate(regija=as.character(regija),regija=case_when(geo=="SE"~"Švedska",T~regija)) %>% select(datum,geo,regija,values) %>% na.omit() %>% group_by(datum,regija) %>% filter(datum>="2000-01-01") %>% summarise(values=mean(values,na.rm = T))
ggplot(pom,aes(x=datum,y=values,col=regija)) + geom_line(size=2) + theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="",title="Već u ožujku opažena snažna kontrakcija prometa u trgovinama",subtitle = "Indeks prometa u maloprodaji, 2015.=100",caption ="Izvor: Eurostat") + scale_x_date(breaks = "1 year",labels = scales::date_format("%y")) + boje_col
# Promet usluge - još nema podataka!
pom <- get_eurostat("sts_setu_m") %>% filter(nace_r2=="H-N_STS" & s_adj=="SCA" & indic_bt=="TOVT" & unit=="I15") %>% mutate(datum = ceiling_date(time,"month")-1) %>% left_join(reg,by="geo") %>% select(datum,geo,regija,values) %>% na.omit() %>% group_by(datum,regija) %>% filter(datum>="2000-01-01") %>% summarise(values=mean(values,na.rm = T))
ggplot(pom,aes(x=datum,y=values,col=regija)) + geom_line(size=2) + theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="",title="Usluge - turnover",caption = paste("Napomena: Djelatnosti H do N.","\nZadnji datum: ",max(pom$datum))) + scale_x_date(breaks = "1 year",labels = scales::date_format("%y")) + boje_col
# Usluge - indeks pouzdanja
pom <- get_eurostat("ei_bsse_m_r2") %>% filter(s_adj=="SA" & indic=="BS-SCI" & unit=="BAL") %>% mutate(datum = ceiling_date(time,"month")-1) %>% left_join(reg,by="geo") %>% mutate(regija=as.character(regija),regija=case_when(geo=="SE"~"Švedska",T~regija)) %>% select(datum,geo,regija,values) %>% na.omit() %>% group_by(datum,regija) %>% filter(datum>="2000-01-01") %>% summarise(values=mean(values,na.rm = T))
ggplot(pom,aes(x=datum,y=values,col=regija)) + geom_line(size=2) + theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="",title="Nezabilježena kontrakcija pouzdanja u uslužnom sektoru",subtitle = "Indeks pouzdanja uslužnog sektora",caption ="Izvor: Eurostat") + scale_x_date(breaks = "1 year",labels = scales::date_format("%y")) + boje_col


# Slika 5. Kratkoročni indikatori potrošačkog raspoloženja - demand strana ####
# Pouzdanje potrošača
pom <- get_eurostat("ei_bsco_m") %>% filter(s_adj=="SA" & indic=="BS-CSMCI" & unit=="BAL") %>% mutate(datum = ceiling_date(time,"month")-1) %>% left_join(reg,by="geo") %>% mutate(regija=as.character(regija),regija=case_when(geo=="SE"~"Švedska",T~regija)) %>% select(datum,geo,regija,values) %>% na.omit() %>% group_by(datum,regija) %>% filter(datum>="2000-01-01") %>% summarise(values=mean(values,na.rm = T))
ggplot(pom,aes(x=datum,y=values,col=regija)) + geom_line(size=2) + theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="",title="Pouzdanje potrošača u slobodnom padu, oporavak u pravilu znatno sporiji",subtitle = "Indeks pouzdanja potrošača",caption ="Izvor: Eurostat") + scale_x_date(breaks = "1 year",labels = scales::date_format("%y")) + boje_col
# Očekivanja potrošača oko ekonomske situacije u sljedećih 12 mjeseci
pom <- get_eurostat("ei_bsco_m") %>% filter(s_adj=="SA" & indic=="BS-GES-NY" & unit=="BAL") %>% mutate(datum = ceiling_date(time,"month")-1) %>% left_join(reg,by="geo") %>% mutate(regija=as.character(regija),regija=case_when(geo=="SE"~"Švedska",T~regija)) %>% select(datum,geo,regija,values) %>% na.omit() %>% group_by(datum,regija) %>% filter(datum>="2000-01-01") %>% summarise(values=mean(values,na.rm = T))
ggplot(pom,aes(x=datum,y=values,col=regija)) + geom_line(size=2) + theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="",title="Očekivanja već sada znatno negativnija nego u prošloj krizi",subtitle = "Indeks očekivanja općenite ekonomske situacije u narednih 12 mjeseci",caption ="Izvor: Eurostat") + scale_x_date(breaks = "1 year",labels = scales::date_format("%y")) + boje_col


# Slika 6. Inflacija i doprinosi ####
# stope inflacije
pom1 <- get_eurostat("prc_hicp_manr") %>% filter(coicop %in% c("CP01","CP02","CP03","CP04","CP05","CP06","CP07","CP08","CP09","CP10","CP11","CP12") & geo=="HR" & unit=="RCH_A") %>% mutate(datum = ceiling_date(time,"month")-1) %>% mutate(dobro=case_when(coicop=="CP01"~"Hrana i piće",coicop=="CP02"~"Alkohol i cigarete",coicop=="CP03"~"Odjeća i obuća",coicop=="CP04"~"Voda, struja i goriva",coicop=="CP05"~"Namještaj",coicop=="CP06"~"Zdravlje",coicop=="CP07"~"Prijevoz",coicop=="CP08"~"Komunikacije",coicop=="CP09"~"Rekreacija i kultura",coicop=="CP10"~"Edukacija",coicop=="CP11"~"Restorani i hoteli",coicop=="CP12"~"Ostala dobra")) %>% filter(month(datum)==12 | datum=="2020-03-31") %>% select(datum,dobro,stopa_rasta=values)
pom1$datum[pom1$datum=="2020-03-31"] <- as.Date("2020-12-31")
# weightovi
pom2 <- get_eurostat(id="prc_hicp_inw") %>% filter(coicop %in% c("CP01","CP02","CP03","CP04","CP05","CP06","CP07","CP08","CP09","CP10","CP11","CP12") & geo=="HR") %>% mutate(datum = ceiling_date(time,"year")-1) %>% mutate(dobro=case_when(coicop=="CP01"~"Hrana i piće",coicop=="CP02"~"Alkohol i cigarete",coicop=="CP03"~"Odjeća i obuća",coicop=="CP04"~"Voda, struja i goriva",coicop=="CP05"~"Namještaj",coicop=="CP06"~"Zdravlje",coicop=="CP07"~"Prijevoz",coicop=="CP08"~"Komunikacije",coicop=="CP09"~"Rekreacija i kultura",coicop=="CP10"~"Edukacija",coicop=="CP11"~"Restorani i hoteli",coicop=="CP12"~"Ostala dobra")) %>% mutate(values=values/1000) %>% select(datum,dobro,ponder=values)
#ukupna inflacija
pom3 <- get_eurostat("prc_hicp_manr") %>% filter(coicop %in% c("CP00") & geo=="HR" & unit=="RCH_A") %>% mutate(datum = ceiling_date(time,"month")-1) %>% filter(month(datum)==12 | datum=="2020-03-31") %>% select(datum,inflacija=values)
pom3$datum[pom3$datum=="2020-03-31"] <- as.Date("2020-12-31")
pom3 <- pom3 %>% mutate(datum=floor_date(datum,unit = "year"))
# sklapanje dobara za doprinose
pom <- inner_join(pom1,pom2,by=c("datum","dobro")) %>% mutate(doprinos=stopa_rasta*ponder) %>% mutate(dobro=case_when(dobro=="Edukacija"~"Ostala dobra",dobro=="Namještaj"~"Ostala dobra",dobro=="Rekreacija i kultura"~"Ostala dobra",dobro=="Zdravlje"~"Ostala dobra",T~dobro)) %>% group_by(datum,dobro) %>% summarise(doprinos=sum(doprinos)) %>% ungroup() %>% mutate(datum=floor_date(datum,unit = "year"))
# grafikon
kriza <- data.frame(begin=as.Date("2007-06-30"),end=as.Date("2010-06-30"))
ggplot(pom,aes(x=datum,y=doprinos)) + geom_col(aes(fill=dobro)) + boje_fill + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_x_date(breaks = "1 year",labels = date_format("%Y")) + labs(x="",y="postotni bodovi",title="U početnom razdoblju krize prevladavaju deflacijski pritisci",subtitle = "doprinosi pojedinačnih kategorija dobara ukupnoj stopi inflacije",caption = "Napomena: Ostala dobra uključuju kategorije: namještaj, edukacija, zdravlje te rekreacija i kultura.\nIzvor: Eurostat") + geom_line(data = pom3,aes(x=datum,y=inflacija),size=1.5) + geom_rect(data = kriza,aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf),inherit.aes = FALSE, fill = "red", alpha = 0.15)

# Slika 7. Cijene nekretnina i doprinosi ####
# stope rasta
pom1 <- get_eurostat("prc_hpi_q") %>% filter(purchase %in% c("DW_NEW","DW_EXST") & geo=="HR" & unit=="RCH_A") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% mutate(tip_nekretnine=case_when(purchase=="DW_NEW"~"Novogradnja",purchase=="DW_EXST"~"Postojeće nekretnine")) %>% select(datum,tip_nekretnine,stopa_rasta=values) %>% mutate(godina = ceiling_date(datum,"year")-1)
# weightovi
pom2 <- get_eurostat(id="prc_hpi_inw") %>% filter(purchase %in% c("DW_NEW","DW_EXST") & geo=="HR") %>% mutate(godina = ceiling_date(time,"year")-1) %>% mutate(tip_nekretnine=case_when(purchase=="DW_NEW"~"Novogradnja",purchase=="DW_EXST"~"Postojeće nekretnine")) %>% mutate(values=values/1000) %>% select(godina,tip_nekretnine,ponder=values)
#ukupni indeks
pom3 <- get_eurostat("prc_hpi_q") %>% filter(purchase %in% c("TOTAL") & geo=="HR" & unit=="RCH_A") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,indeks=values) %>% mutate(datum=floor_date(datum,unit = "quarter"))
# sklapanje za doprinose
pom <- inner_join(pom1,pom2,by=c("godina","tip_nekretnine")) %>% mutate(doprinos=stopa_rasta*ponder)  %>% ungroup() %>% mutate(datum=floor_date(datum,unit = "quarter"))
ggplot(pom,aes(x=datum,y=doprinos)) + geom_col(aes(fill=tip_nekretnine)) + boje_fill + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_x_date(breaks = "1 year",labels = date_format("%Y")) + labs(x="",y="postotni bodovi",title="Cijene postojeće nekretnina brže se korigiraju",subtitle = "doprinosi tipova nekretnina ukupnoj godišnjoj stopi promjene cijena",caption = "Izvor: Eurostat") + geom_line(data = pom3,aes(x=datum,y=indeks),size=1.5)

# Slika 7.b. Indeksi cijena ukupni, novi, postojeći
pom <- get_eurostat("prc_hpi_q") %>% filter(unit=="RCH_A") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% mutate(tip_nekretnine=case_when(purchase=="DW_NEW"~"Novogradnja",purchase=="DW_EXST"~"Postojeće nekretnine",T~"Ukupno")) %>% select(datum,geo,tip_nekretnine,stopa_rasta=values) %>% left_join(reg,by="geo") %>% group_by(datum,regija,tip_nekretnine) %>% summarise(stopa_rasta=mean(stopa_rasta,na.rm = T)) %>% na.omit()
ggplot(pom,aes(x=datum,y=stopa_rasta,col=tip_nekretnine)) + geom_line(size=1.5) + facet_wrap(~regija,scales = "free") + boje_col

# Slika 8. Nekretnine ####
# 8.a. Indeksi cijena
pom <- read_excel(path = "D:/mbamba/skripte/corona_sim/popratni_podaci/nekretnine.xlsx") %>% gather("regija","indeks",-datum) %>% mutate(datum=as.Date(datum))
ggplot(pom,aes(x=datum,y=indeks,col=regija)) + geom_line(size=2) + boje_col + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_x_date(breaks = "1 year",labels = date_format("%Y")) + labs(x="",y="indeks, 2015. = 100",title="Rast cijena koncentriran u glavnom gradu i na obali",subtitle = "indeks cijena stambenih nekretnina",caption = "Izvor: DZS")
# 8.b. Homeownership vs. procyclicality 
pom1 <- get_eurostat("prc_hpi_q") %>% filter(purchase=="TOTAL" & unit=="INX_Q") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% filter(datum>="2007-12-31" & datum<="2013-12-31") %>% group_by(geo) %>% summarise(maksimum=max(values,na.rm=T),minimum=min(values,na.rm=T)) %>% mutate(range=maksimum-minimum)
pom2 <- get_eurostat(id="ilc_lvho02") %>% filter(tenure=="OWN" & incgrp=="TOTAL" & hhtyp=="TOTAL")%>% mutate(datum = ceiling_date(time,"year")-1) %>% filter(datum>="2007-12-31" & datum<="2013-12-31") %>% group_by(geo) %>% summarise(ownership=mean(values,na.rm = T))
pom <- inner_join(pom1,pom2,by="geo") %>% filter(!geo %in% c("EA","EA19","EU","EU27_2020","EU28")) %>% left_join(reg,by="geo") %>% na.omit()
ggplot(pom,aes(x=ownership,y=range)) + geom_point(size=4,aes(col=regija)) + geom_text(inherit.aes = T,aes(label=geo),nudge_x = 1.5) + geom_smooth(method = "lm",se=F)+ boje_col + labs(x="Posjedovanje nekretnine (prosjek 2008.-2013., u % ukupne populacije)",y="Pad cijena nekretnina u razdoblju 2008. - 2013.",title = "Izraženije posjedovanje nekretnina u protekloj krizi djelovao prociklično",caption = "Napomena: Pad cijena nekretnina procijenjen je kao razlika najveće i najmanje vrijednosti u promatranom razdoblju.\nIzvor: Eurostat")




# Slika 20. Alluvial financijski računi ####
load("D:/mbamba/dwh/Financijski_racuni/FinRacuni.Rda")
pom <- financijski_racuni %>% filter(!protustrana_mb %in% c("Total","NA")) %>% mutate(protustrana_mb=case_when(protustrana_mb=="Poduzeća"~"POD",protustrana_mb=="Ostale FI"~"FI",protustrana_mb=="Centralna banka"~"DRŽ",protustrana_mb=="Kreditne institucije"~"KI",protustrana_mb=="Investicijski fondovi"~"FI",protustrana_mb=="Osiguranja"~"FI",protustrana_mb=="Mirovinski fondovi"~"FI",protustrana_mb=="Država"~"DRŽ",protustrana_mb=="Stanovništvo"~"STA",protustrana_mb=="Inozemstvo"~"INO")) %>% mutate(razina=case_when(razina=="Assets"~"Imovina",razina=="Liabilities"~"Obveze")) %>% group_by(sektor_mb,protustrana_mb,razina) %>% filter(datum=="2019-12-31" & sektor_mb %in% c("Mirovinski fondovi","Investicijski fondovi","Osiguranja","Ostale FI") & vrsta_iznosa=="BAL T" & vrsta_imovine!="Total") %>% summarise(iznos=sum(iznos,na.rm = T)) %>% mutate(iznos=iznos/1000000000) %>% ungroup()%>% mutate(sektor_mb=case_when(sektor_mb=="Investicijski fondovi"~"Inv. fondovi",sektor_mb=="Mirovinski fondovi"~"Mir. fondovi",T~sektor_mb))
ggplot(pom,aes(y=iznos,axis1=protustrana_mb,axis2=sektor_mb)) + facet_wrap(~razina)+ geom_alluvium(aes(fill=protustrana_mb)) +geom_stratum(width = 1/8,alpha=0.5) + geom_text(stat="stratum",infer.label=T,size=3.5) + theme(legend.position = "top") + boje_fill + scale_x_continuous(breaks=1:2,labels=c("Protustrana","Sektor")) + labs(x="",y="mlrd. HRK",title = "Kretanja u sektorima stanovništva i države ponajviše će utjecati na financijske usluge",subtitle = "Prikaz međupovezanosti sektora financijskih usluga i ostalih sektora ekonomije, stanje na 31.12.2019.",caption = "Izvori: HNB, Hanfa")
