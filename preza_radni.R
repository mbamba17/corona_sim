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
reg = data.frame(geo=c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE","GB"),country=c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Rep.","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom"),regija=c("Ostale zemlje EU","Ostale zemlje EU","Zemlje SIE","HR","Ostale zemlje EU","Zemlje SIE","Ostale zemlje EU","Zemlje SIE","Ostale zemlje EU","Ostale zemlje EU","Ostale zemlje EU","Ostale zemlje EU","Zemlje SIE","Ostale zemlje EU","Ostale zemlje EU","Zemlje SIE","Zemlje SIE","Ostale zemlje EU","Ostale zemlje EU","Ostale zemlje EU","Zemlje SIE","Ostale zemlje EU","Zemlje SIE","Zemlje SIE","Zemlje SIE","Ostale zemlje EU","Ostale zemlje EU","Ostale zemlje EU"))

# 1. COVID-19 podaci ####
library(rjson)
library(jsonlite)
covid <- fromJSON("https://api.covid19api.com/all")
covid <- covid %>% mutate(datum=as.Date(Date))
save(covid,file="covid.Rda")
pom1 <- covid %>% filter(CountryCode %in% c("HR","CN","US","IT","ES","DE","UK","SE","RU")) %>% select(datum,geo=CountryCode,Confirmed,Deaths,Recovered,Active) %>% group_by(datum,geo) %>% summarise(Slučajevi=sum(Confirmed,na.rm=T),Preminuli=sum(Deaths,na.rm=T),Oporavljeni=sum(Recovered,na.rm=T),Aktivni=sum(Active,na.rm=T))
pom2 <- data.frame(geo=c("HR","CN","US","IT","ES","DE","UK","SE","RU"),population=c(4.089,1392,327.167,60.431,46.724,82.928,66.489,10.183,144.478))
pom <- left_join(pom1,pom2,by="geo") %>% mutate(`Slučajevi`=`Slučajevi`/population,Preminuli=Preminuli/population,Oporavljeni=Oporavljeni/population,Aktivni=Aktivni/population) %>% select(-population)%>% gather(value = "broj",key = "varijabla",-datum,-geo) %>% filter(datum>="2020-03-01")
ggplot(pom,aes(x=datum,y=broj,col=geo)) + geom_line(size=1.2) + facet_wrap(~varijabla,scales="free") + boje_col + labs(x="",y="",subtitle="Broj slučajeva na milijun stanovnika",title="Zdravstvena kriza u većini zemalja prošla vrhunac",caption="Izvor: John Hopkins CSSE")
rm(pom1,pom2,covid)

# 2. Lockdown tracker ####

lockdown <- rio::import(file="https://covid19-lockdown-tracker.netlify.com/lockdown_dates.csv")
pom <- lockdown %>% filter((Country %in% reg$country | Country %in% c("Czech Rep.")) & Level=="National") %>% select(geo=Country,start=`Start date`,end=`End date`) %>% mutate(start=ymd(start),end=ymd(end))
ggplot(pom) + geom_segment(aes(x=start,xend=end,y=geo,yend=geo),size=1.05,col="#155e63") + geom_point(size=3,aes(x=start,y=geo),col="#E84545") + geom_point(size=3,aes(x=end,y=geo),col="#01D28E") + labs(x="",y="",title="EU zemlje prelaze u fazu popuštanja epidemioloških mjera",subtitle = "Razdoblje ograničenja kretanja od uvođenja mjera do (najavljenog) početka popuštanja",caption = "Izvor: Aura Vision") + geom_vline(xintercept = as.Date("2020-05-15"),col="#ffc93c")
rm(lockdown)

# 3. Stringency index ####
stringency <- rio::import(file="https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv")
pom <- stringency %>% select(country=CountryName,indeks=StringencyIndex) %>% group_by(country) %>% summarise(indeks=max(indeks,na.rm=T)) %>% filter(country %in% reg$country | country %in% c("Czech Republic","Slovak Republic","Albania","Bosnia and Herzegovina","Switzerland","Iceland","Moldova","Norway","Serbia","Turkey","Ukraine","Kosovo")) %>% mutate(country=case_when(country=="Czech Republic"~"Czech Rep.",country=="Slovak Republic"~"Slovakia",country=="Bosnia and Herzegovina"~"Bosnia and Herz.",T~country))
library(grid)
library(rworldmap)
library(mapproj)
# Get the world map
worldMap <- getMap()
# Select only the index of states member of the E.U.
indEU <- which(worldMap$NAME %in% pom$country)
# Extract longitude and latitude border's coordinates of members states of E.U. 
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})
europeCoords <- do.call("rbind", europeCoords)
# crtanje grafikona
europeanUnionTable <- data.frame(country = pom$country, value = pom$indeks)
europeCoords$value <- europeanUnionTable$value[match(europeCoords$region,europeanUnionTable$country)]
# grafikon
ggplot() + geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = value), colour = "black", size = 0.1) + theme(panel.grid.minor = element_line(colour = NA), panel.background = element_rect(fill = NA, colour = NA), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), axis.title = element_blank(), rect = element_blank(),legend.position = "right") + scale_fill_gradient(name = "Kumulativni rast BDP-a od 2010.", low ="#ffc93c" , high ="#E84545", guide = "legend", na.value = "grey50") + labs(title="U zemljama na jugu europe znatno jače epidemiološke mjere",subtitle = "maksimalna vrijednost indeksa strogosti epidemioloških mjera, siječanj 2020. - svibanj 2020.",caption="Napomena: Viša vrijednost indeksa označava strožije epidemiološke mjere.\nIzvor: Oxford COVID 19 Goverment Response Tracker")
rm(worldMap,europeanUnionTable,europeCoords,indEU,stringency)

# 4. Google mobility data ####
mobility <- rio::import(file="https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=722f3143b586a83f")
pom <- mobility %>% filter(sub_region_1=="") %>% select(geo=country_region_code,country=country_region,datum=date,`Uslužne i kulturne lokacije`=retail_and_recreation_percent_change_from_baseline,`Trgovine i ljekarne`=grocery_and_pharmacy_percent_change_from_baseline,Parkovi=parks_percent_change_from_baseline,`Tranzitne stanice`=transit_stations_percent_change_from_baseline,`Radna mjesta`=workplaces_percent_change_from_baseline,`Mjesto prebivališta`=residential_percent_change_from_baseline) %>% gather(key = "lokacija",value = "promjena",-datum,-geo,-country) %>% filter(geo=="HR") %>% mutate(datum=as.Date(datum))
# prikaz za hrvatsku
ggplot(pom,aes(x=datum,y=promjena,col=lokacija)) + geom_line(size=2) + facet_wrap(~lokacija) + boje_col + geom_vline(xintercept = as.Date("2020-03-18"), col="darkred", size=1.2, linetype=2) + geom_vline(xintercept = as.Date("2020-05-04"), col="darkgreen", size=1.2, linetype=2) + theme(legend.position = "none")+ labs(x="",y="",subtitle="Relativna promjena u odnosu na redovnu vrijednost (%)",caption="Napomena: Redovna vrijednost označava medijalnu vrijednost u razdoblju od 3. siječnja do 6. veljače 2020.\nIzvor: Google mobility data", title = "Mobilnost građana smanjila se čak i prije uvođenja karantene")

# 5. Šok supply strane - trgovine ####
# Promet trgovine
pom <- get_eurostat("sts_trtu_m") %>% filter(nace_r2=="G47" & s_adj=="SCA" & indic_bt=="TOVV" & unit=="I15") %>% mutate(datum = ceiling_date(time,"month")-1) %>% left_join(reg,by="geo") %>% mutate(regija=as.character(regija),regija=case_when(geo=="SE"~"Švedska",T~regija)) %>% select(datum,geo,regija,values) %>% na.omit() %>% group_by(datum,regija) %>% filter(datum>="2000-01-01") %>% summarise(values=mean(values,na.rm = T))
ggplot(subset(pom,datum<"2020-03-31"),aes(x=datum,y=values,col=regija)) + geom_line(size=2) + geom_point(data = subset(pom,datum=="2020-03-31"),size=4)+ theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="",title="Već u ožujku opažena snažna kontrakcija prometa u trgovinama",subtitle = "Indeks prometa u maloprodaji, 2015.=100",caption ="Izvor: Eurostat") + scale_x_date(breaks = "1 year",labels = scales::date_format("%y")) + boje_col
# 6. Šok supply strane - usluge ####
# Promet usluge - još nema podataka!
pom <- get_eurostat("sts_setu_m") %>% filter(nace_r2=="H-N_STS" & s_adj=="SCA" & indic_bt=="TOVT" & unit=="I15") %>% mutate(datum = ceiling_date(time,"month")-1) %>% left_join(reg,by="geo") %>% select(datum,geo,regija,values) %>% na.omit() %>% group_by(datum,regija) %>% filter(datum>="2000-01-01") %>% summarise(values=mean(values,na.rm = T))
ggplot(pom,aes(x=datum,y=values,col=regija)) + geom_line(size=2) + theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="",title="Usluge - turnover",caption = paste("Napomena: Djelatnosti H do N.","\nZadnji datum: ",max(pom$datum))) + scale_x_date(breaks = "1 year",labels = scales::date_format("%y")) + boje_col
# Usluge - indeks pouzdanja
pom <- get_eurostat("ei_bsse_m_r2") %>% filter(s_adj=="SA" & indic=="BS-SCI" & unit=="BAL") %>% mutate(datum = ceiling_date(time,"month")-1) %>% left_join(reg,by="geo") %>% mutate(regija=as.character(regija),regija=case_when(geo=="SE"~"Švedska",T~regija)) %>% select(datum,geo,regija,values) %>% na.omit() %>% group_by(datum,regija) %>% filter(datum>="2000-01-01") %>% summarise(values=mean(values,na.rm = T))
ggplot(subset(pom,datum<="2020-03-31"),aes(x=datum,y=values,col=regija)) + geom_line(size=2) + geom_point(data=subset(pom,datum>"2020-03-31"),size=4)+ theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="",title="Nezabilježena kontrakcija pouzdanja u uslužnom sektoru",subtitle = "Indeks pouzdanja uslužnog sektora",caption ="Izvor: Eurostat") + scale_x_date(breaks = "1 year",labels = scales::date_format("%y")) + boje_col

# 7. Šok demand strana - pouzdanje ####
pom <- get_eurostat("ei_bsco_m") %>% filter(s_adj=="SA" & indic=="BS-CSMCI" & unit=="BAL") %>% mutate(datum = ceiling_date(time,"month")-1) %>% left_join(reg,by="geo") %>% mutate(regija=as.character(regija),regija=case_when(geo=="SE"~"Švedska",T~regija)) %>% select(datum,geo,regija,values) %>% na.omit() %>% group_by(datum,regija) %>% filter(datum>="2000-01-01") %>% summarise(values=mean(values,na.rm = T))
ggplot(subset(pom,datum<="2020-03-31"),aes(x=datum,y=values,col=regija)) + geom_line(size=2) + geom_point(data=subset(pom,datum>"2020-03-31"),size=4)+ theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="",title="Pouzdanje potrošača u slobodnom padu, oporavak u pravilu znatno sporiji",subtitle = "Indeks pouzdanja potrošača",caption ="Izvor: Eurostat") + scale_x_date(breaks = "1 year",labels = scales::date_format("%y")) + boje_col

# 8. Šok demand strana - očekivanje ####
pom <- get_eurostat("ei_bsco_m") %>% filter(s_adj=="SA" & indic=="BS-GES-NY" & unit=="BAL") %>% mutate(datum = ceiling_date(time,"month")-1) %>% left_join(reg,by="geo") %>% mutate(regija=as.character(regija),regija=case_when(geo=="SE"~"Švedska",T~regija)) %>% select(datum,geo,regija,values) %>% na.omit() %>% group_by(datum,regija) %>% filter(datum>="2000-01-01") %>% summarise(values=mean(values,na.rm = T))
ggplot(subset(pom,datum<="2020-03-31"),aes(x=datum,y=values,col=regija)) + geom_line(size=2) + geom_point(data=subset(pom,datum>"2020-03-31"),size=4)+ theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="",title="Očekivanja već sada znatno negativnija nego u prošloj krizi",subtitle = "Indeks očekivanja općenite ekonomske situacije u narednih 12 mjeseci",caption ="Izvor: Eurostat") + scale_x_date(breaks = "1 year",labels = scales::date_format("%y")) + boje_col

# 9. Fiskalni impuls ####
pom <- read_excel(path = "D:/mbamba/skripte/corona_sim/popratni_podaci/Fiskalni impuls.xlsx",sheet = "usporedba",range = "A1:E14") %>% arrange(Total) %>% mutate(geo_fct=factor(geo,levels = geo)) %>% select(-geo,-Total) %>% gather(key = "vrsta",value = "iznos",-geo_fct) 
ggplot(pom,aes(x=geo_fct,y=iznos,fill=vrsta)) + geom_col() + boje_fill + scale_y_continuous(labels = scales::percent)+ labs(x="",y="",title= "Domaći fiskalni odgovor na krizu relativno skroman u odnosu na zapadne zemlje",subtitle = "Veličina fiskalnog odgovora na COVID-19 krizu, u % BDP-a 2019.",caption = "Napomena: Prikazane su samo mjere koje je moguće kvantitativno odrediti.\nIzvori: DZS, Ministarstvo financija, Bruegel.")

# 10. HZZ potpore ####
library(rjson)
library(jsonlite)
hzz_podaci <- fromJSON("https://mjera-orm.hzz.hr/korisnici-potpore/ozujak-2020/json/")$Data
hzz_podaci <- hzz_podaci %>% select(Oib,Broj=SupportedEmployeeNumber,Iznos=SupportPaidAmount)
pom <- hzz_podaci %>% mutate(kategorija=case_when(Broj<=5~"Do 5 zaposlenika",Broj>5 & Broj<=10~"6 do 10 zaposlenika",Broj>10 & Broj<=15~"11 do 15 zaposlenika",Broj>15 & Broj<=25~"16 do 25 zaposlenika",Broj>25 & Broj<=50~"26 do 50 zaposlenika",Broj>50 & Broj<=250~"51 do 250 zaposlenika",T~"Više od 250 zaposlenika")) %>% mutate(kategorija=factor(kategorija,levels=c("Do 5 zaposlenika","6 do 10 zaposlenika","11 do 15 zaposlenika","16 do 25 zaposlenika","26 do 50 zaposlenika","51 do 250 zaposlenika","Više od 250 zaposlenika"))) %>% mutate(velicina=case_when(kategorija %in% c("Do 5 zaposlenika","6 do 10 zaposlenika")~"Mikro",kategorija %in% c("11 do 15 zaposlenika","16 do 25 zaposlenika","26 do 50 zaposlenika")~"Mala",kategorija=="51 do 250 zaposlenika"~"Srednja",kategorija=="Više od 250 zaposlenika"~"Velika"))
ggplot(pom,aes(x=kategorija,y=Broj,fill=velicina)) + geom_col() + boje_fill + labs(x="",y="Broj zaposlenika",caption="Izvor: HZZ",title = "Mjere potpore za očuvanje radnih mjesta najviše koriste mikro poduzeća", subtitle = "Broj korisnika mjere za očuvanje radnih mjesta, prema veličini poduzeća, ožujak 2020.") + scale_y_continuous(labels = scales::comma) + scale_x_discrete(labels = scales::wrap_format(10))

# 11. Shematski prikaz simulacije ####
pom <- get_eurostat(id="namq_10_gdp") %>% filter(geo=="HR" & na_item=="B1GQ" & s_adj=="NSA" & unit=="CLV10_MNAC") %>% select(time,bdp=values) %>% arrange(time) %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% as.data.frame() %>% mutate(bdp_y=bdp+lag(bdp,1)+lag(bdp,2)+lag(bdp,3),dbdp=(bdp_y/lag(bdp_y,4)-1)*100) %>% mutate(dbdp_m=(dbdp+lag(dbdp,1)+lag(dbdp,2)+lag(dbdp,3)+lag(dbdp,4)+lag(dbdp,5)+lag(dbdp,6)+lag(dbdp,7))/8) %>% select(datum,dbdp_m) %>% na.omit() %>% filter(datum>="2007-12-31")
pom1 <- data.frame(datum=rep(c("2019-12-31","2020-03-31","2020-06-30","2020-09-30","2020-12-31"),3),scenarij=c(rep("Scenarij1",5),rep("Scenarij2",5),rep("Scenarij3",5)),dbdp_m=c(2.8958114,-1,-2,-4,-5,2.8958114,-1.5,-3,-6,-7,2.8958114,-2,-4,-7,-9),raspon=rep(c(0,0.5,1,2,3),3)) %>% mutate(datum=as.Date(datum)) %>% mutate(min_int=dbdp_m-raspon,max_int=dbdp_m+raspon)
ggplot(pom,aes(x=datum,y=dbdp_m)) + geom_line(size=2) + geom_line(data = pom1,size=2,aes(x=datum,y=dbdp_m,col=scenarij)) + geom_ribbon(data = pom1,aes(ymin=min_int,ymax=max_int,fill=scenarij),alpha=0.2)+ labs(x="",y="Godišnja stopa rasta realnog BDP-a (%)",title="Projekcija efekata trenutne krize sa sobom nose veliku maržu pogreške",subtitle="Shematski prikaz pouzdanosti projekcije utjecaja COVID-19 krize na gospodarstvo",caption="Napomena: Kretanje BDP-a u različitim scenarijima prikazano je isključivo u ilustrativne svrhe.\nIzvor: Hanfa") + boje_col + boje_fill
rm(pom1)

# 12. U dobrim vremenima svi konvergiraju, a u lošim divegiraju ####
pom <- get_eurostat(id="namq_10_gdp") %>% filter(na_item=="B1GQ" & s_adj=="NSA" & unit=="CLV10_MNAC" & !geo %in% c("EA","EA12","EA19","EU15","EU27_2020","EU28")) %>% select(time,geo,bdp=values) %>% arrange(time) %>% as.data.frame() %>% group_by(geo) %>% mutate(bdp_y = (bdp + lag(bdp,1)+ lag(bdp,2) + lag(bdp,3)),dbdp=(bdp_y/lag(bdp_y,4)-1)*100) %>% mutate(datum = ceiling_date(time,"month")-1) %>% select(datum,geo,dbdp) %>% left_join(reg,by="geo") %>% na.omit() %>% group_by(datum) %>% summarise(pc_10=quantile(dbdp,probs = 0.1,na.rm = T),pc_90=quantile(dbdp,probs = 0.9,na.rm = T),dbdp=mean(dbdp,na.rm=T)) %>% mutate(iqr=pc_90-pc_10) %>% ungroup() %>% filter(datum>"1990-12-31")
ggplot(pom,aes(x=datum,y=dbdp)) + geom_ribbon(aes(ymin=pc_10,ymax=pc_90), fill = "#155e63",alpha=0.4) + geom_ribbon(aes(ymin=-20,ymax=iqr-20), fill = "#e84545",alpha=0.4) + geom_line(size=1.5) + labs(x="",y="Godišnja stopa rasta (%)",title="U kriznim razdobljima raste divergencija",subtitle="Raspon gospodarskog rasta 10% najboljih i najlošijih europskih zemalja naglo raste u kriznim razdobljima",caption = "Izvor: Eurostat") + scale_x_date(date_labels = "%Y.",breaks = "5 years") + boje_col + scale_y_continuous(limits = c(-20,15),breaks = seq(-20,15,by=5),sec.axis = sec_axis(~ . + 20, name = derive()))

# 13. Struktura BDP-a rashodovna metoda ####

# doprinosi rastu
pom1 <- get_eurostat(id="namq_10_gdp") %>% filter(s_adj=="SCA" & na_item %in% c("P31_S14_S15","P3_S13","P5G","P6","P7") & unit=="CLV10_MEUR") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,geo,sektor=na_item,values) %>% mutate(sektor=case_when(sektor=="P31_S14_S15"~"Potrošnja kućanstava",sektor=="P3_S13"~"Državna potrošnja",sektor=="P5G"~"Bruto investicije",sektor=="P6"~"Izvoz",sektor=="P7"~"Uvoz")) %>% inner_join(reg,by="geo") %>% group_by(datum,regija,sektor) %>% summarise(values=sum(values,na.rm=T)) %>% spread(sektor,values) %>% mutate(`Neto izvoz`=Izvoz-Uvoz) %>% gather(key = "sektor",value = "values",-datum,-regija) %>% filter(!sektor %in% c("Uvoz","Izvoz")) %>% group_by(regija,sektor) %>% mutate(bdp_y=values+lag(values,1)+lag(values,2)+lag(values,3)) %>% select(datum,regija,sektor,bdp_y)
pom2 <- pom1 %>% group_by(datum,regija) %>% summarise(bdp_total=sum(bdp_y,na.rm=T))
pom <- inner_join(pom1,pom2,by=c("datum","regija")) %>% mutate(udio=bdp_y/bdp_total) %>% mutate(doprinos=(bdp_y/lag(bdp_y,4)-1)*100*lag(udio,4))%>% filter(datum>="2001-12-31" & datum<="2019-12-31")
pom2 <- pom2 %>% group_by(regija) %>% mutate(dbdp=(bdp_total/lag(bdp_total,4)-1)*100) %>% filter(datum>="2001-12-31" & datum<="2019-12-31")
ggplot(pom,aes(x=datum,y=doprinos)) + geom_col(alpha=0.8,aes(fill=sektor))  + geom_line(data = pom2,aes(x=datum,y=dbdp),size=1.5) + facet_wrap(~regija) + boje_fill + labs(x="",y="postotni bodovi",title = "Gospodarski rast generira osobna potrošnja, no dubinu kontrakcije određuju investicije",subtitle = "Doprinosi pojedinih djelatnosti realnom rastu bruto domaćeg proizvoda",caption = "Izvor: Eurostat") + scale_y_continuous(breaks = -7:7)
rm(pom1,pom2)

# 14. Struktura BDP-a proizvodna metoda ####
pom1 <- get_eurostat(id="namq_10_a10") %>% filter(s_adj=="SCA" & na_item=="B1G" & unit=="CLV10_MEUR") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,geo,sektor=nace_r2,values) %>% filter(!sektor %in% c("TOTAL","C")) %>% mutate(sektor=case_when(sektor=="A"~"Poljoprivreda",sektor=="B-E"~"Industrija",sektor=="F"~"Građevinarstvo",sektor=="G-I"~"Trgovina, prijevoz i smještaj",sektor=="J"~"Ostale sluge",sektor=="K"~"Financije i promet nekretninama",sektor=="L"~"Financije i promet nekretninama",sektor=="M_N"~"Ostale usluge",sektor=="O-Q"~"Javna uprava",sektor=="R-U"~"Ostale usluge")) %>% inner_join(reg,by="geo") %>% group_by(datum,regija,sektor) %>% summarise(values=sum(values,na.rm=T)) %>% group_by(regija,sektor) %>% mutate(bdv_y=values+lag(values,1)+lag(values,2)+lag(values,3)) %>% select(datum,regija,sektor,bdv_y)
pom2 <- pom1 %>% group_by(datum,regija) %>% summarise(bdv_total=sum(bdv_y,na.rm=T))
pom <- inner_join(pom1,pom2,by=c("datum","regija")) %>% mutate(udio=bdv_y/bdv_total) %>% mutate(doprinos=(bdv_y/lag(bdv_y,4)-1)*100*lag(udio,4))%>% filter(datum>="2001-12-31" & datum<="2019-12-31")
pom2 <- pom2 %>% group_by(regija) %>% mutate(dbdv=(bdv_total/lag(bdv_total,4)-1)*100) %>% filter(datum>="2001-12-31" & datum<="2019-12-31")
ggplot(pom,aes(x=datum,y=doprinos)) + geom_col(alpha=0.8,aes(fill=sektor))  + geom_line(data = pom2,aes(x=datum,y=dbdv),size=1.5) + facet_wrap(~regija) + boje_fill + labs(x="",y="postotni bodovi",title = "Najveći dio rasta BDV-a generiraju trgovina i uslužne djelatnosti",subtitle = "Doprinosi pojedinih djelatnosti realnom rastu bruto dodane vrijednosti",caption = "Izvor: Eurostat") + scale_y_continuous(breaks = -7:7)
rm(pom1,pom2)

# 15. Turizam - dolazak stranaca ####
pom <- get_eurostat(id="tour_occ_nim") %>% filter(c_resid!="TOTAL" & nace_r2=="I551-I553" & unit=="NR" & geo=="HR") %>% filter(time>="2012-01-01") %>% mutate(values=values/1000000,c_resid=case_when(c_resid=="FOR"~"Strani turisti",c_resid=="NAT"~"Domaći turisti")) %>% mutate(c_resid=as_factor(c_resid))
pom$c_resid <- factor(pom$c_resid, levels = c("Strani turisti","Domaći turisti"))
ggplot(pom,aes(x=time,y=values,fill=c_resid)) + geom_area() + scale_y_continuous(labels = comma) + theme(legend.position = "top") + labs(x="",y="",title="Domaći turizam ovisi o dolasku i potrošačkoj moći stranaca",subtitle = "Prikaz broja noćenja turista po rezidentnosti, u milijunima",caption = "Izvor: Eurostat") + boje_fill

# 16. Turizam - Vrste smještaja ####
pom <- get_eurostat(id="tour_occ_nim") %>% filter(c_resid=="TOTAL" & nace_r2 %in% c("I551","I552","I551") & unit=="NR" & geo=="HR") %>% mutate(godina=year(time),nace_r2=case_when(nace_r2=="I551"~"Hoteli",nace_r2=="I553"~"Kampovi",nace_r2=="I552"~"Ostali smještaj")) %>% group_by(nace_r2,godina) %>% summarise(values=sum(values,na.rm=T)) %>% filter(godina>=2012 & godina<2020) %>% mutate(values=values/1000000)
pom$nace_r2 <- factor(pom$nace_r2, levels = c("Ostali smještaj","Hoteli"))
ggplot(pom,aes(x=godina,y=values,fill=nace_r2)) + geom_col() + scale_y_continuous(labels = comma) + theme(legend.position = "top") + labs(x="",y="",title="Ponuda najvećim dijelom dolazi u obliku privatnog smještaja",subtitle = "Prikaz broja noćenja turista po vrsti smještaja, u milijunima",caption = "Izvor: Eurostat") + boje_fill

# 17. Prikaz veličine šoka na BDP ####
pom <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "bdp",range = "bm42:bp62") %>% gather("scenarij","iznos",-sektor)
ggplot(pom,aes(x=sektor,y=iznos,fill=scenarij)) + geom_col(position = "dodge") + boje_fill  + scale_y_continuous(labels = percent)+ theme(axis.text.x = element_text(angle = 90, hjust = 0,vjust = 0)) + labs(x="",y="",title = "Prikaz simuliranih vrijednosti bruto dodane vrijednosti",subtitle="godišnja stopa promjene, po djelatnostima",caption = "Izvori: DZS, Hanfa")

# 18. BDP ####
pom1 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "bdp",range = "bx2:by82") %>% mutate(datum=as.Date(datum))
pom2 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "bdp",range = "bx84:ca89")%>% gather("scenarij","iznos",-datum) %>% mutate(datum=as.Date(datum))
pom3 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "bdp",range = "bx91:ca92")%>% gather("scenarij","iznos",-datum) %>% mutate(datum=as.Date(datum)) %>%  mutate(izns=round(iznos*100,1)) %>% select(datum,scenarij,izns)
pom3 <- left_join(pom3,pom2,by=c("datum","scenarij"))
ggplot(pom1,aes(x=datum,y=iznos)) + geom_line(size=2) + geom_line(data = pom2,size=2,aes(x=datum,y=iznos,col=scenarij)) + labs(x="",y="",title="Bruto domaći proizvod",subtitle="godišnja stopa rasta (%)",caption="Napomena: Numerički je označena stopa promjene godišnjeg bruto domaćeg proizvoda.\nIzvor: Hanfa") + boje_col + geom_text(data=subset(pom3, datum == "2020-12-31"),aes(label=izns,col=scenarij),hjust=0, vjust=0, show.legend = FALSE) + scale_y_continuous(labels = percent) + scale_x_date(breaks = "1 year",labels = date_format("%y"),limits = c(as.Date("2001-01-01"),as.Date("2021-06-01")))

# 19. Usporedba scenarija sa ostvarenjima zemalja za Q1 2020 ####

pom1 <- get_eurostat(id="namq_10_a10") %>% filter(s_adj=="SCA" & na_item=="B1G" & unit=="CP_MNAC" & geo %in% c("AT","ES","FR","NO")) %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,geo,sektor=nace_r2,values) %>% arrange(datum) %>% group_by(geo,sektor)%>% mutate(dbdv=round((values/lag(values,4)-1)*100,1)) %>% filter(datum=="2020-03-31") %>% select(-datum,-values) %>% spread(geo,dbdv)
pom2 <- read_excel(path = "D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "bdp",range = "BM4:BP16")
pom <- left_join(pom1,pom2,by="sektor") %>% gather(key = "geo",value = "dbdv",-sektor) %>% mutate(regija=case_when(!geo %in% c("S1","S2","S3")~"bla",T~geo)) %>% ungroup() %>% mutate(sektor=case_when(sektor=="A"~"Poljoprivreda",sektor=="B-E"~"Industrija",sektor=="C"~"Prerađivačka industrija",sektor=="F"~"Građevinarstvo",sektor=="G-I"~"Trgovina, prijevoz i smještaj",sektor=="J"~"Informacije",sektor=="K"~"Financije",sektor=="L"~"Promet nekretninama",sektor=="M_N"~"Znanost i administracija",sektor=="O-Q"~"Javna uprava",sektor=="R-U"~"Ostale usluge",sektor=="TOTAL"~"Ukupno"))
ggplot(pom,aes(x=geo,y=dbdv,fill=regija)) + geom_col() + facet_wrap(~sektor,scales="free",ncol = 3,shrink = T) + scale_x_discrete(labels = scales::wrap_format(10)) + theme(legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1)) + boje_fill + labs(x="",y="",title = "Usporedba simuliranog BDV s ostvarenjima pojedinih zemalja",subtitle = "bruto dodana vrijednost po djelatnostima, godišnja stopa rasta 1. tromjesečje 2020.",caption = "Izvori: Eurostat, Hanfa")

# 20. Usporedba sa rezultatima porezne ####
# 1. Prošlogodišnji rezultati
pom1 <- read_excel(path="D:/mbamba/skripte/corona_sim/popratni_podaci/porezna_podaci.xlsx",sheet = "mjesecno") %>% mutate(datum=as.Date(datum)) %>% filter(datum=="2018-04-30" | datum=="2019-04-30" | datum=="2020-04-30") %>% group_by(datum,naziv1) %>% summarise(iznos=sum(iznos,na.rm=T)) %>% spread(datum,iznos) %>% mutate(`2019`=(`2019-04-30`/`2018-04-30`-1)*100,`2020`=(`2020-04-30`/`2019-04-30`-1)*100) %>% select(sektor=naziv1,`2019`,`2020`)
pom2 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "bdp",range = "BM19:BO39")
pom <- left_join(pom1,pom2,by="sektor") %>% gather(key = "scenarij",value = "dbdv",-sektor)
ggplot(pom,aes(x=scenarij,y=dbdv,fill=scenarij)) + geom_col(position = "dodge")  + boje_fill + facet_wrap(~sektor) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x="",y="godišnja stopa promjene (%)",title="Usporedba kalibracije scenarija sa podacima iz PU",subtitle = "godišnje stope rasta BDV po djelatnostima, 1. tromjesečje 2020.",caption="Izvor: Eurostat")
# 2. Scenarij vs travanj 2020.
pom1 <- read_excel(path="D:/mbamba/skripte/corona_sim/popratni_podaci/porezna_podaci.xlsx",sheet = "mjesecno") %>% mutate(datum=as.Date(datum)) %>% filter(datum=="2019-04-30" | datum=="2020-04-30") %>% group_by(datum,naziv1) %>% summarise(iznos=sum(iznos,na.rm=T)) %>% spread(datum,iznos) %>% mutate(`2020`=(`2020-04-30`/`2019-04-30`-1)*100) %>% select(sektor=naziv1,`2020`)
pom2 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "bdp",range = "BM19:BR39") %>% filter(ukljuciti=="da") %>% select(sektor,S1,S2,S3)
pom <- inner_join(pom1,pom2,by="sektor") %>% gather(key = "scenarij",value = "dbdv",-sektor)
ggplot(pom,aes(x=scenarij,y=dbdv,fill=scenarij)) + geom_col(position = "dodge")  + boje_fill + facet_wrap(~sektor,ncol = 2,scales = "free_y") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x="",y="godišnja stopa promjene (%)",title="Usporedba kalibracije scenarija sa podacima iz PU",subtitle = "usporedba šoka karantene s godišnjom stopom promjene iznosa računa iz PU, travanj 2020.",caption="Izvori: Porezna uprava, Hanfa")

# 21. Stopa nezaposlenosti ####
pom1 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "nezaposlenost",range = "u3:x87") %>% mutate(datum=as.Date(datum)) %>% filter(datum<="2019-12-31") %>% select(datum,iznos=Scenarij1)
pom2 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "nezaposlenost",range = "u3:x87") %>% mutate(datum=as.Date(datum)) %>% filter(datum>="2019-12-31") %>% gather("scenarij","iznos",-datum) %>%  mutate(izns=round(iznos,1))
ggplot(pom1,aes(x=datum,y=iznos)) + geom_line(size=2) + geom_line(data = pom2,size=2,aes(x=datum,y=iznos,col=scenarij)) + labs(x="",y="",title="Stopa nezaposlenosti",subtitle="Desezonirana vrijednost, administrativni izvori",caption="Izvor: Hanfa") + boje_col + geom_text(data=subset(pom2, datum == "2020-12-31"),aes(label=izns,col=scenarij),hjust=0, vjust=0, show.legend = FALSE) +  scale_x_date(breaks = "1 year",labels = date_format("%y"),limits = c(as.Date("2001-01-01"),as.Date("2021-06-01")))

# 22. Plaće ####
pom1 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "place",range = "x2:aa94") %>% mutate(datum=as.Date(datum)) %>% filter(datum<="2019-12-31") %>% select(datum,iznos=Scenarij1)
pom2 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "place",range = "x2:aa94") %>% mutate(datum=as.Date(datum)) %>% filter(datum>="2019-12-31") %>% gather("scenarij","iznos",-datum) %>%  mutate(izns=round(iznos,0))
ggplot(pom1,aes(x=datum,y=iznos)) + geom_line(size=2) + geom_line(data = pom2,size=2,aes(x=datum,y=iznos,col=scenarij)) + labs(x="",y="",title="Nominalne bruto plaće",subtitle="Desezonirana vrijednost, administrativni izvori",caption="Izvor: Hanfa") + boje_col + geom_text(data=subset(pom2, datum == "2020-12-31"),aes(label=izns,col=scenarij),hjust=0, vjust=0, show.legend = FALSE,labels=comma) +  scale_x_date(breaks = "1 year",labels = date_format("%y"),limits = c(as.Date("2001-01-01"),as.Date("2021-06-01")))

# 23. Javne financije ####
# 1. Prihodi, rashodi i poslovni rezultat
pom1 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "javne_financije",range = "AG51:AJ69") %>% gather("varijabla","iznos",-datum) %>% mutate(datum=as.Date(datum))
pom2 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "javne_financije",range = "AG72:AK75")%>% gather("varijabla","iznos",-datum,-scenarij)  %>% mutate(datum=as.Date(datum)) %>%  mutate(izns=round(iznos/1000,1))
ggplot(pom1,aes(x=datum,y=iznos)) + geom_line(size=2) + geom_point(data = pom2,size=4,aes(x=datum,y=iznos,col=scenarij),) + facet_wrap(~varijabla,scales = "free") + labs(x="",y="",title="Prihodi i rashodi opće države",subtitle="u mil. HRK",caption="Izvor: Hanfa") + boje_col + scale_y_continuous(labels = comma) + geom_text(data=pom2,aes(label=izns,col=scenarij),hjust=1.4, vjust=0.4,show.legend = FALSE)

# 24. Deficit i javni dug
pom1 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "javne_financije",range = "AL51:AN69") %>% gather("varijabla","iznos",-datum) %>% mutate(datum=as.Date(datum))
pom2 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "javne_financije",range = "AM72:AP75")%>% gather("varijabla","iznos",-datum,-scenarij)  %>% mutate(datum=as.Date(datum)) %>% filter(datum>"2019-12-31") %>% mutate(izns=round(iznos*100,1))
ggplot(pom1,aes(x=datum,y=iznos)) + geom_line(size=2) + geom_point(data = pom2,size=4,aes(x=datum,y=iznos,col=scenarij)) + facet_wrap(~varijabla,scales = "free") + labs(x="",y="",title="Javni dug i deficit",subtitle = "u % BDP-a",caption="Izvor: Hanfa") + boje_col + scale_y_continuous(labels = percent) + geom_text(data=pom2,aes(label=izns,col=scenarij),hjust=1.4, vjust=0.4,show.legend = FALSE)

# 25. Tekući račun platne bilance ####
pom1 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "vanjski_sektor",range = "x2:y82") %>% mutate(datum=as.Date(datum))
pom2 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "vanjski_sektor",range = "x84:aa89")%>% gather("scenarij","iznos",-datum) %>% mutate(datum=as.Date(datum)) %>%  mutate(izns=round(iznos*100,1))
ggplot(pom1,aes(x=datum,y=iznos)) + geom_line(size=2) + geom_line(data = pom2,size=2,aes(x=datum,y=iznos,col=scenarij)) + labs(x="",y="",title="Tekući račun platne bilance",subtitle="u % BDP-a",caption="Izvor: Hanfa") + boje_col + geom_text(data=subset(pom2, datum == "2020-12-31"),aes(label=izns,col=scenarij),hjust=-0.01, vjust=0, show.legend = FALSE) + scale_y_continuous(labels = percent) 

# 26.Tečaj ####
pom1 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "tecaj",range = "M2:N82") %>% mutate(datum=as.Date(datum))
pom2 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "tecaj",range = "M84:P89")%>% gather("scenarij","iznos",-datum) %>% mutate(datum=as.Date(datum)) %>%  mutate(izns=round(iznos,2))
ggplot(pom1,aes(x=datum,y=iznos)) + geom_line(size=2) + geom_line(data = pom2,size=2,aes(x=datum,y=iznos,col=scenarij)) + labs(x="",y="",title="Tečaj EUR/HRK",subtitle="",caption="Izvor: Hanfa") + boje_col + geom_text(data=subset(pom2, datum == "2020-12-31"),aes(label=izns,col=scenarij),hjust=-0.1, vjust=0, show.legend = FALSE) 
# 27. Inflacija i doprinosi ####
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
#kriza <- data.frame(begin=as.Date("2007-06-30"),end=as.Date("2010-06-30"))
ggplot(pom,aes(x=datum,y=doprinos)) + geom_col(aes(fill=dobro)) + boje_fill + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_x_date(breaks = "1 year",labels = date_format("%Y")) + labs(x="",y="postotni bodovi",title="U kratkom roku prevladavaju deflacijski pritisci",subtitle = "Doprinosi pojedinačnih kategorija dobara ukupnoj stopi inflacije",caption = "Napomena: Ostala dobra uključuju kategorije: namještaj, edukacija, zdravlje te rekreacija i kultura.\nIzvor: Eurostat") + geom_line(data = pom3,aes(x=datum,y=inflacija),size=1.5)
rm(pom1,pom2,pom3,kriza)

# Slika 28. Inflacija ####
pom1 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "inflacija",range = "x2:y78") %>% mutate(datum=as.Date(datum))
pom2 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "inflacija",range = "x80:aa85")%>% gather("scenarij","iznos",-datum) %>% mutate(datum=as.Date(datum)) %>%  mutate(izns=round(iznos,1))
ggplot(pom1,aes(x=datum,y=iznos)) + geom_line(size=2) + geom_line(data = pom2,size=2,aes(x=datum,y=iznos,col=scenarij)) + labs(x="",y="",title="Inflacija",subtitle="godišnja stopa rasta (%)",caption="Izvor: Hanfa") + boje_col + geom_text(data=subset(pom2, datum == "2020-12-31"),aes(label=izns,col=scenarij),hjust=-0.01, vjust=0, show.legend = FALSE) 

# Slika 29. Krediti HH&NFC ####
pom1 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "krediti",range = "af2:ai105")  %>% mutate(datum=as.Date(datum)) %>% filter(datum<="2020-03-31") %>% select(datum,iznos=Scenarij1)
pom2 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "krediti",range = "af2:ai105") %>% mutate(datum=as.Date(datum)) %>% filter(datum>="2020-03-31") %>% gather("scenarij","iznos",-datum) %>%  mutate(izns=round(iznos,1))
ggplot(pom1,aes(x=datum,y=iznos)) + geom_line(size=2) + geom_line(data = pom2,size=2,aes(x=datum,y=iznos,col=scenarij)) + labs(x="",y="",title="Krediti realnom sektoru",subtitle="Godišnja stopa rasta kredita banaka sektorima kućanstava i nefinancijskih poduzeća (%)",caption="Izvor: Hanfa") + boje_col + geom_text(data=subset(pom2, datum == "2020-12-31"),aes(label=izns,col=scenarij),hjust=0, vjust=0, show.legend = FALSE) +  scale_x_date(breaks = "1 year",labels = date_format("%y"),limits = c(as.Date("2001-01-01"),as.Date("2021-01-01"))) + scale_y_continuous(limits = c(-10,35))

# Slika 30. Kamatne stope ####
pom1 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "kstope",range = "r2:u78")  %>% mutate(datum=as.Date(datum)) %>% filter(datum<="2020-03-31") %>% select(datum,iznos=Scenarij1)
pom2 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "kstope",range = "r2:u78") %>% mutate(datum=as.Date(datum)) %>% filter(datum>="2020-03-31") %>% gather("scenarij","iznos",-datum) %>%  mutate(izns=round(iznos,1))
ggplot(pom1,aes(x=datum,y=iznos)) + geom_line(size=2) + geom_line(data = pom2,size=2,aes(x=datum,y=iznos,col=scenarij)) + labs(x="",y="",title="Kamatne stope na kredite realnom sektoru",subtitle="Kamatna stopa na stanja dugoročnih kredita banaka sektorima kućanstava i nefinancijskih poduzeća (%)",caption="Izvor: Hanfa") + boje_col + geom_text(data=subset(pom2, datum == "2020-12-31"),aes(label=izns,col=scenarij),hjust=0, vjust=0, show.legend = FALSE) +  scale_x_date(breaks = "1 year",labels = date_format("%y"),limits = c(as.Date("2006-01-01"),as.Date("2021-01-01")))

# 31. Nekretnine - indeksi ####
pom <- read_excel(path = "D:/mbamba/skripte/corona_sim/popratni_podaci/nekretnine.xlsx") %>% gather("regija","indeks",-datum) %>% mutate(datum=as.Date(datum))
ggplot(pom,aes(x=datum,y=indeks,col=regija)) + geom_line(size=2) + boje_col + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_x_date(breaks = "1 year",labels = date_format("%Y")) + labs(x="",y="",title="Rast cijena koncentriran u glavnom gradu i na obali",subtitle = "Indeks cijena stambenih nekretnina, 2015. = 100",caption = "Izvor: DZS")

# 32. Nekretnine ####
pom1 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "nekretnine",range = "y2:z70") %>% mutate(datum=as.Date(datum))
pom2 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "nekretnine",range = "y72:ab77")%>% gather("scenarij","iznos",-datum) %>% mutate(datum=as.Date(datum)) %>%  mutate(izns=round(iznos,1))
ggplot(pom1,aes(x=datum,y=iznos)) + geom_line(size=2) + geom_line(data = pom2,size=2,aes(x=datum,y=iznos,col=scenarij)) + labs(x="",y="",title="Cijene nekretnina",subtitle="godišnja stopa rasta (%)",caption="Izvor: Hanfa") + boje_col + geom_text(data=subset(pom2, datum == "2020-12-31"),aes(label=izns,col=scenarij),hjust=0, vjust=0, show.legend = FALSE) 

# 33. Prinosi ####
pom1 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "prinos",range = "am2:ap78") %>% mutate(datum=as.Date(datum)) %>% filter(datum<="2020-03-31") %>% select(datum,iznos=Scenarij1)
pom2 <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "prinos",range = "am2:ap78")%>% mutate(datum=as.Date(datum)) %>% filter(datum>="2020-03-31") %>% gather("scenarij","iznos",-datum) %>%  mutate(izns=round(iznos,1))
ggplot(pom1,aes(x=datum,y=iznos)) + geom_line(size=2) + geom_line(data = pom2,size=2,aes(x=datum,y=iznos,col=scenarij)) + labs(x="",y="",title="Prinosi na državne obveznice",subtitle="Prinosi do dospijeća na dugoročne obveznice RH (%)",caption="Izvor: Hanfa") + boje_col + geom_text(data=subset(pom2, datum == "2020-12-31"),aes(label=izns,col=scenarij),hjust=0, vjust=0, show.legend = FALSE) 

# 34. Dionice ####
pom <- read_excel("D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "dionice",range = "p1:q159") %>% mutate(datum=as.Date(datum)) %>% filter(datum>="2019-01-01")
ggplot(pom,aes(x=datum,y=iznos)) + geom_line(size=2) + labs(x="",y="",title="CROBEX indeks",subtitle="Projicirano kretanje vrijednosti CROBEX indeksa",caption="Napomena: Vertikalna linija označava posljednji opaženi podatak.\nIzvor: Hanfa") + boje_col + geom_vline(xintercept = ymd("2020-04-30"),col="red",size=1.5,linetype=2)

# 36. Alluvial financijski računi ####
load("D:/mbamba/dwh/Financijski_racuni/FinRacuni.Rda")
pom <- financijski_racuni %>% filter(!protustrana_mb %in% c("Total","NA")) %>% mutate(protustrana_mb=case_when(protustrana_mb=="Poduzeća"~"POD",protustrana_mb=="Ostale FI"~"FI",protustrana_mb=="Centralna banka"~"DRŽ",protustrana_mb=="Kreditne institucije"~"KI",protustrana_mb=="Investicijski fondovi"~"FI",protustrana_mb=="Osiguranja"~"FI",protustrana_mb=="Mirovinski fondovi"~"FI",protustrana_mb=="Država"~"DRŽ",protustrana_mb=="Stanovništvo"~"STA",protustrana_mb=="Inozemstvo"~"INO")) %>% mutate(razina=case_when(razina=="Assets"~"Imovina",razina=="Liabilities"~"Obveze")) %>% group_by(sektor_mb,protustrana_mb,razina) %>% filter(datum=="2019-12-31" & sektor_mb %in% c("Mirovinski fondovi","Investicijski fondovi","Osiguranja","Ostale FI") & vrsta_iznosa=="BAL T" & vrsta_imovine!="Total") %>% summarise(iznos=sum(iznos,na.rm = T)) %>% mutate(iznos=iznos/1000000000) %>% ungroup()%>% mutate(sektor_mb=case_when(sektor_mb=="Investicijski fondovi"~"Inv. fondovi",sektor_mb=="Mirovinski fondovi"~"Mir. fondovi",T~sektor_mb))
ggplot(pom,aes(y=iznos,axis1=protustrana_mb,axis2=sektor_mb)) + facet_wrap(~razina)+ geom_alluvium(aes(fill=protustrana_mb)) +geom_stratum(width = 1/8,alpha=0.5) + geom_text(stat="stratum",infer.label=T,size=3.5) + theme(legend.position = "top") + boje_fill + scale_x_continuous(breaks=1:2,labels=c("Protustrana","Sektor")) + labs(x="",y="mlrd. HRK",title = "Kretanja u sektorima stanovništva i države ponajviše će utjecati na financijske usluge",subtitle = "Prikaz međupovezanosti sektora financijskih usluga i ostalih sektora ekonomije, stanje na 31.12.2019.",caption = "Izvori: HNB, Hanfa")
rm(financijski_racuni)











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
rm(pom1,pom2,pom3)

# Slika 7.b. Indeksi cijena ukupni, novi, postojeći
pom <- get_eurostat("prc_hpi_q") %>% filter(unit=="RCH_A") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% mutate(tip_nekretnine=case_when(purchase=="DW_NEW"~"Novogradnja",purchase=="DW_EXST"~"Postojeće nekretnine",T~"Ukupno")) %>% select(datum,geo,tip_nekretnine,stopa_rasta=values) %>% left_join(reg,by="geo") %>% group_by(datum,regija,tip_nekretnine) %>% summarise(stopa_rasta=mean(stopa_rasta,na.rm = T)) %>% na.omit()
ggplot(pom,aes(x=datum,y=stopa_rasta,col=tip_nekretnine)) + geom_line(size=1.5) + facet_wrap(~regija,scales = "free") + boje_col

# 8.b. Homeownership vs. procyclicality 
pom1 <- get_eurostat("prc_hpi_q") %>% filter(purchase=="TOTAL" & unit=="INX_Q") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% filter(datum>="2007-12-31" & datum<="2013-12-31") %>% group_by(geo) %>% summarise(maksimum=max(values,na.rm=T),minimum=min(values,na.rm=T)) %>% mutate(range=maksimum-minimum)
pom2 <- get_eurostat(id="ilc_lvho02") %>% filter(tenure=="OWN" & incgrp=="TOTAL" & hhtyp=="TOTAL")%>% mutate(datum = ceiling_date(time,"year")-1) %>% filter(datum>="2007-12-31" & datum<="2013-12-31") %>% group_by(geo) %>% summarise(ownership=mean(values,na.rm = T))
pom <- inner_join(pom1,pom2,by="geo") %>% filter(!geo %in% c("EA","EA19","EU","EU27_2020","EU28")) %>% left_join(reg,by="geo") %>% na.omit()
ggplot(pom,aes(x=ownership,y=range)) + geom_point(size=4,aes(col=regija)) + geom_text(inherit.aes = T,aes(label=geo),nudge_x = 1.5) + geom_smooth(method = "lm",se=F)+ boje_col + labs(x="Posjedovanje nekretnine (prosjek 2008.-2013., u % ukupne populacije)",y="Pad cijena nekretnina u razdoblju 2008. - 2013.",title = "Izraženije posjedovanje nekretnina u protekloj krizi djelovao prociklično",caption = "Napomena: Pad cijena nekretnina procijenjen je kao razlika najveće i najmanje vrijednosti u promatranom razdoblju.\nIzvor: Eurostat")
rm(pom1,pom2)

# 20. Prikaz rezultata scenarija ####
pom <- read_excel(path = "D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "sazetak",range="A2:R15") %>% gather(key = "godina",value = "iznos",-`Makroekonomski indikator`) %>% mutate(kategorija=case_when(!godina %in% c("Scenarij1","Scenarij2","Scenarij3")~"bla",T~godina))
ggplot(pom,aes(x=godina,y=iznos,group=`Makroekonomski indikator`,fill=kategorija)) + geom_col() + facet_wrap(~`Makroekonomski indikator`,scales="free",ncol = 3,shrink = T) + scale_x_discrete(labels = scales::wrap_format(10)) + theme(legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1)) + boje_fill + labs(x="",y="",title = "Makroekonomski scenarij",subtitle = "vrijednosti ključnih makroekonomskih varijabli u različitim scenarijima na kraju prosinca 2020.",caption = "Izvor: Hanfa")









# 15. Industrija vs. turizam (u % BDV-a) ####
pom <- get_eurostat(id = "nama_10_a10") %>% filter(na_item %in% c("B1G") & nace_r2 %in% c("TOTAL","B-E","G-I") & unit=="CP_MEUR" & time=="2019-01-01" & !geo %in% c("EU15","EU27_2020","EU28","EA","EA12","EA19")) %>% select(geo,nace_r2,values) %>% spread(nace_r2,values) %>% mutate(industrija=`B-E`/TOTAL,usluge=(`G-I`)/TOTAL) %>% left_join(reg,by="geo") %>% na.omit()
ggplot(pom,aes(x=industrija,y=usluge)) + geom_point(size=4,aes(col=regija)) + geom_text(inherit.aes = T,aes(label=geo),nudge_x = 0.009) + geom_smooth(method = "lm",se=F)+ boje_col + labs(x="Industrija",y="Trgovina i uslužne djelatnosti",title = "Visoko oslanjanje domaće ekonomije na uslužne djelatnosti",subtitle="Udio pojedinih djelatnosti u ukupnoj bruto dodanoj vrijednosti, 2019.",caption = "Napomena: Industrija uključuje  djelatnosti B, C, D i E, dok trgovina i usluge uključuje djelatnosti G,H i I.\nIzvor: Eurostat")

# 16. Nezaposlenost - udio part time radnika ####
pom <- get_eurostat(id = "lfsi_pt_a") %>% filter(unit=="THS_PER" & time=="2019-01-01" & sex=="T" & age=="Y15-64") %>% group_by(geo,worktime) %>% summarise(values=sum(values)) %>% spread(worktime,values) %>% mutate(ukupno=PT+TEMP,udio=PT/ukupno)

# 17. Struktura poduzeća - udio SME ####
# Turnover
pom <- get_eurostat(id = "sbs_sc_sca_r2") %>% filter(((time=="2016-01-01" & geo %in% c("AT","FR","IT")) | (time=="2017-01-01" & !geo %in% c("AT","FR","IT"))) & nace_r2=="B-N_S95_X_K" & indic_sb=="V12110" & size_emp!="TOTAL") %>% mutate(size_emp=case_when(size_emp=="0-9"~"Mikro",size_emp %in% c("10-19","20-49")~"Mala",size_emp=="50-249"~"Srednja",size_emp=="GE250"~"Velika")) %>% left_join(reg,by="geo") %>% na.omit() %>% filter(!geo %in% c("CY","EL","PT")) %>% group_by(geo,size_emp,regija) %>% summarise(values=sum(values,na.rm=T)) %>% spread(size_emp,values) %>% mutate(sme_udio=(Mala+Srednja)/(Mikro+Mala+Srednja+Velika)*100) %>% arrange(sme_udio) %>% mutate(geo_fct=factor(geo,levels = geo))
ggplot(pom,aes(x=reorder(geo,-sme_udio),y=sme_udio,fill=regija))+ geom_col() + boje_fill
# Broj zaposlenih
pom <- get_eurostat(id = "sbs_sc_sca_r2") %>% filter(((time=="2016-01-01" & geo %in% c("AT","FR","IT")) | (time=="2017-01-01" & !geo %in% c("AT","FR","IT"))) & nace_r2=="B-N_S95_X_K" & indic_sb=="V16110" & size_emp!="TOTAL") %>% mutate(size_emp=case_when(size_emp=="0-9"~"Mikro",size_emp %in% c("10-19","20-49")~"Mala",size_emp=="50-249"~"Srednja",size_emp=="GE250"~"Velika")) %>% left_join(reg,by="geo") %>% na.omit() %>% filter(!geo %in% c("CY","EL","PT")) %>% group_by(geo,size_emp,regija) %>% summarise(values=sum(values,na.rm=T)) %>% spread(size_emp,values) %>% mutate(sme_udio=(Mala+Srednja)/(Mikro+Mala+Srednja+Velika)*100) %>% arrange(sme_udio) %>% mutate(geo_fct=factor(geo,levels = geo))
ggplot(pom,aes(x=reorder(geo,-sme_udio),y=sme_udio,fill=regija))+ geom_col() + boje_fill
# Dodana vrijednost
pom <- get_eurostat(id = "sbs_sc_sca_r2") %>% filter(((time=="2016-01-01" & geo %in% c("AT","FR","IT")) | (time=="2017-01-01" & !geo %in% c("AT","FR","IT"))) & nace_r2=="B-N_S95_X_K" & indic_sb=="V12150" & size_emp!="TOTAL") %>% mutate(size_emp=case_when(size_emp=="0-9"~"Mikro",size_emp %in% c("10-19","20-49")~"Mala",size_emp=="50-249"~"Srednja",size_emp=="GE250"~"Velika")) %>% left_join(reg,by="geo") %>% na.omit() %>% filter(!geo %in% c("CY","EL","PT")) %>% group_by(geo,size_emp,regija) %>% summarise(values=sum(values,na.rm=T)) %>% spread(size_emp,values) %>% mutate(sme_udio=(Mala+Srednja)/(Mikro+Mala+Srednja+Velika)*100) %>% arrange(sme_udio) %>% mutate(geo_fct=factor(geo,levels = geo))
ggplot(pom,aes(x=reorder(geo,-sme_udio),y=sme_udio,fill=regija))+ geom_col() + boje_fill


# 18. Struktura BDP-a proizvodna strana ####
# u mlrd. HRK
pom <- get_eurostat(id="namq_10_a10") %>% filter(s_adj=="SCA" & na_item=="B1G" & unit=="CP_MNAC" & geo=="HR") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,sektor=nace_r2,values) %>% filter(sektor!="TOTAL") %>% mutate(sektor=case_when(sektor=="A"~"Poljoprivreda, šumarstvo i ribarstvo",sektor=="B-E"~"Industrija i rudarstvo",sektor=="C"~"Prerađivačka industrija",sektor=="F"~"Građevinarstvo",sektor=="G-I"~"Trgovina, prijevoz i smještaj",sektor=="J"~"Informacije i komunikacije",sektor=="K"~"Financijske djelatnosti",sektor=="L"~"Poslovanje nekretninama",sektor=="M_N"~"Stručne i administrativne djelatnosti",sektor=="O-Q"~"Javna uprava",sektor=="R-U"~"Ostale uslužne djelatnosti")) %>% spread(sektor,values) %>% mutate(`Industrija i rudarstvo`=`Industrija i rudarstvo`-`Prerađivačka industrija`) %>% gather(key = sektor,value = values,-datum)
pom1 <- data.frame(begin=as.Date("2020-01-01"),end=as.Date("2020-12-31"))
ggplot(pom,aes(x=datum,y=values,fill=sektor)) + geom_area() + boje_fill + labs(x="",y="mil. HRK",title = "Rastući udio trgovine i usluga u ukupnoj gospodarksoj aktivnosti",subtitle = "struktura buto dodane vrijednosti prema djelatnostima",caption = "Izvor: Eurostat") + guides(fill = guide_legend(ncol = 3)) + scale_y_continuous(labels = comma) + scale_x_date(date_labels = "%y.",breaks = "1 year",limits = c(as.Date("2000-12-31"),as.Date("2020-12-31"))) + geom_rect(data = pom1,aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf),inherit.aes = FALSE, fill = "red", alpha = 0.15)


# 20. Prikaz rezultata scenarija ####
pom <- read_excel(path = "D:/mbamba/skripte/corona_sim/radni.xlsx",sheet = "sazetak",range="A2:R13") %>% gather(key = "godina",value = "iznos",-indikator) %>% mutate(kategorija=case_when(!godina %in% c("Scenarij1","Scenarij2","Scenarij3")~"bla",T~godina))
ggplot(pom,aes(x=godina,y=iznos,group=indikator,fill=kategorija)) + geom_col() + facet_wrap(~indikator,scales="free",ncol = 3,shrink = T) + scale_x_discrete(labels = scales::wrap_format(10)) + theme(legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1)) + boje_fill + labs(x="",y="",title = "Makroekonomski scenarij",subtitle = "vrijednosti ključnih makroekonomskih varijabli u različitim scenarijima na kraju prosinca 2020.",caption = "Izvor: Hanfa")




