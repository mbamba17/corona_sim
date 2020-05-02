library(tidyverse)
library(scales)
library(ggthemes)
library(extrafont)
library(eurostat)
library(lubridate)
library(ecb)

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
reg = data.frame(geo=c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE","GB"),country=c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Rep.","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom"),regija=c("Other EU","Other EU","CEE","HR","Other EU","CEE","Other EU","CEE","Other EU","Other EU","Other EU","Other EU","CEE","Other EU","Other EU","CEE","CEE","Other EU","Other EU","Other EU","CEE","Other EU","CEE","CEE","CEE","Other EU","Other EU","Other EU"))

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
pom <- get_eurostat(id="namq_10_a10_e") %>% filter(s_adj=="SCA" & na_item=="EMP_DC" & unit=="THS_PER") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,sektor=nace_r2,values) %>% filter(sektor!="TOTAL") %>% mutate(sektor=case_when(sektor=="A"~"Poljoprivreda, šumarstvo i ribarstvo",sektor=="B-E"~"Industrija i rudarstvo",sektor=="C"~"Prerađivačka industrija",sektor=="F"~"Građevinarstvo",sektor=="G-I"~"Trgovina, prijevoz, smještaj, priprema hrane",sektor=="J"~"Informacije i komunikacije",sektor=="K"~"Financijske djelatnosti",sektor=="L"~"Poslovanje nekretninama",sektor=="M_N"~"Stručne i administrativne djelatnosti",sektor=="O-Q"~"Javna uprava",sektor=="R-U"~"Ostale uslužne djelatnosti")) %>% spread(sektor,values) %>% mutate(`Industrija i rudarstvo`=`Industrija i rudarstvo`-`Prerađivačka industrija`) %>% gather(key = sektor,value = values,-datum)
ggplot(pom,aes(x=datum,y=values,fill=sektor)) + geom_area() + boje_fill
# u %
pom <- get_eurostat(id="namq_10_a10_e") %>% filter(s_adj=="SCA" & na_item=="EMP_DC" & unit=="PC_TOT_PER" & geo=="HR") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,sektor=nace_r2,values) %>% filter(sektor!="TOTAL") %>% mutate(sektor=case_when(sektor=="A"~"Poljoprivreda, šumarstvo i ribarstvo",sektor=="B-E"~"Industrija i rudarstvo",sektor=="C"~"Prerađivačka industrija",sektor=="F"~"Građevinarstvo",sektor=="G-I"~"Trgovina, prijevoz, smještaj, priprema hrane",sektor=="J"~"Informacije i komunikacije",sektor=="K"~"Financijske djelatnosti",sektor=="L"~"Poslovanje nekretninama",sektor=="M_N"~"Stručne i administrativne djelatnosti",sektor=="O-Q"~"Javna uprava",sektor=="R-U"~"Ostale uslužne djelatnosti")) %>% spread(sektor,values) %>% mutate(`Industrija i rudarstvo`=`Industrija i rudarstvo`-`Prerađivačka industrija`) %>% gather(key = sektor,value = values,-datum)
ggplot(pom,aes(x=datum,y=values,fill=sektor)) + geom_area() + boje_fill
# usporedba po zemljama
pom <- get_eurostat(id="namq_10_a10_e") %>% filter(s_adj=="SCA" & na_item=="EMP_DC" & unit=="PC_TOT_PER") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,geo,sektor=nace_r2,values) %>% filter(sektor!="TOTAL" & datum==max(datum)) %>% mutate(sektor=case_when(sektor=="A"~"Poljoprivreda, šumarstvo i ribarstvo",sektor=="B-E"~"Industrija i rudarstvo",sektor=="C"~"Prerađivačka industrija",sektor=="F"~"Građevinarstvo",sektor=="G-I"~"Trgovina, prijevoz, smještaj, priprema hrane",sektor=="J"~"Informacije i komunikacije",sektor=="K"~"Financijske djelatnosti",sektor=="L"~"Poslovanje nekretninama",sektor=="M_N"~"Stručne i administrativne djelatnosti",sektor=="O-Q"~"Javna uprava",sektor=="R-U"~"Ostale uslužne djelatnosti")) %>% spread(sektor,values) %>% mutate(`Industrija i rudarstvo`=`Industrija i rudarstvo`-`Prerađivačka industrija`) %>% gather(key = sektor,value = values,-datum,-geo)
ggplot(pom,aes(x=geo,y=values,fill=sektor)) + geom_col() + boje_fill

# 5. Odnos BDV-a i broja zaposlenih po djelatnostima ####
pom1 <- get_eurostat(id="namq_10_a10") %>% filter(s_adj=="NSA" & na_item=="B1G" & unit=="CP_MNAC" & nace_r2!="TOTAL" & geo %in% reg$geo) %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,geo,sektor=nace_r2,bdv=values) %>% group_by(geo,sektor) %>% mutate(dbdv=(bdv/lag(bdv,4)-1)*100)
pom2 <- get_eurostat(id="namq_10_a10_e") %>% filter(s_adj=="NSA" & na_item=="EMP_DC" & unit=="THS_PER" & nace_r2!="TOTAL" & geo %in% reg$geo) %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,geo,sektor=nace_r2,emp=values) %>% group_by(geo,sektor) %>% mutate(demp=(emp/lag(emp,4)-1)*100)
pom <- left_join(pom1,pom2,by=c("datum","geo","sektor")) %>% left_join(reg,by="geo") %>% mutate(razdoblje=case_when(year(datum)==2008~"2008.",year(datum)==2009~"2009.",year(datum)==2010~"2010.",T~"ostale godine"))

# emp
ggplot(pom,aes(x=datum,y=emp,col=sektor)) + geom_line(size=1.5) + facet_wrap(~geo,scales = "free") + boje_col
ggplot(pom %>% filter(geo=="HR"),aes(x=datum,y=emp,col=sektor)) + geom_line(size=1.5) + boje_col
# bdv
ggplot(pom,aes(x=datum,y=dbdv,col=sektor)) + geom_line(size=1.5) + facet_wrap(~geo,scales = "free") + boje_col
ggplot(pom %>% filter(geo=="HR"),aes(x=datum,y=dbdv,col=sektor)) + geom_line(size=1.5) + boje_col

# scatter
ggplot(pom,aes(x=demp,y=dbdv,col=razdoblje)) + geom_point(alpha=ifelse(pom$razdoblje!="ostale godine",1,0.3)) + facet_wrap(~sektor, scales="free") + boje_col + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + theme(legend.position = "top") + geom_smooth(method = "lm",se = F)
ggplot(pom %>% filter(razdoblje!="ostale godine"),aes(x=demp,y=dbdv,col=razdoblje)) + geom_point(alpha=0.5) + facet_wrap(~sektor, scales="free") + boje_col + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + theme(legend.position = "top") + geom_smooth(method = "lm",se = F)

# panel regresija po industriji
library(plm)
pom1 <- get_eurostat(id="namq_10_a10") %>% filter(s_adj=="NSA" & na_item=="B1G" & unit=="CP_MNAC" & nace_r2!="TOTAL" & geo %in% reg$geo) %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,geo,sektor=nace_r2,bdv=values) %>% group_by(geo,sektor) %>% mutate(dbdv=(bdv/lag(bdv,4)-1)*100)
pom2 <- get_eurostat(id="namq_10_a10_e") %>% filter(s_adj=="NSA" & na_item=="EMP_DC" & unit=="THS_PER" & nace_r2!="TOTAL" & geo %in% reg$geo) %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,geo,sektor=nace_r2,emp=values) %>% group_by(geo,sektor) %>% mutate(demp=(emp/lag(emp,4)-1)*100)
pom <- left_join(pom1,pom2,by=c("datum","geo","sektor")) %>% left_join(reg,by="geo") 
# selektiranje uzorka
uzorak = pom %>% filter(datum>="2008-12-31" & datum<="2013-12-31" & regija %in% c("CEE","HR"))

sektori <- unique(pom$sektor)
rezultati <- data.frame(sektor=sektori) %>% mutate(alfa=NA,beta=NA)
for(i in 1:length(sektori)){
  temp <- uzorak %>% ungroup() %>% filter(sektor==sektori[i]) %>% select(datum,geo,dbdv,demp)
  temp <- pdata.frame(temp,index = c("geo","datum"))
  reza <- pool <- plm(formula = demp~dbdv , data = temp, index = c("geo","datum"), model = "within")
  rezultati$alfa[i] <- fixef(reza)[names(fixef(reza))=="HR"]
  rezultati$beta[i] <- reza$coefficients[1]
}
skopiraj(rezultati)
rm(i,temp,reza)

# 6. Javne financije ####

# 6.1. Rashodi
rzn <- data.frame(na_item=c("D1","D3","P2_D29_D5_D8","D4","D62_D632","D7","D9","OP5ANP"),razina2=c("Compensation of employees","Subsidies","Intermediate consumption","Property income","Social benefits","Other current transfers","Capital transfers","Gross capital formation"))
sektor1=c("Total","General public services","General public services","General public services","General public services","General public services","General public services","General public services","General public services","General public services","Defence","Defence","Defence","Defence","Defence","Defence","Public order and safety","Public order and safety","Public order and safety","Public order and safety","Public order and safety","Public order and safety","Public order and safety","Economic affairs","Economic affairs", "Economic affairs", "Economic affairs","Economic affairs", "Economic affairs", "Economic affairs","Economic affairs", "Economic affairs", "Economic affairs","Environmental protection","Environmental protection","Environmental protection","Environmental protection","Environmental protection","Environmental protection","Environmental protection","Housing and community amenities","Housing and community amenities","Housing and community amenities","Housing and community amenities","Housing and community amenities","Housing and community amenities","Housing and community amenities","Health","Health","Health","Health","Health","Health","Health","Recreation, culture and religion","Recreation, culture and religion","Recreation, culture and religion","Recreation, culture and religion","Recreation, culture and religion","Recreation, culture and religion","Recreation, culture and religion","Education","Education","Education","Education","Education","Education","Education","Education","Education","Social protection","Social protection","Social protection","Social protection","Social protection","Social protection","Social protection","Social protection","Social protection","Social protection")
sektor2=c(NA,NA,"Executive and legislative organs, financial and fiscal affairs, external affairs","Foreign economic aid","General services","Basic research","R&D General public services","General public services n.e.c.","Public debt transactions","Transfers of a general character between different levels of government",NA,"Military defence","Civil defence","Foreign military aid","R&D Defence","Defence n.e.c.",NA,"Police services","Fire-protection services","Law courts","Prisons","R&D Public order and safety","Public order and safety n.e.c.",NA,"General economic, commercial and labour affairs","Agriculture, forestry, fishing and hunting","Fuel and energy","Mining, manufacturing and construction","Transport","Communication","Other industries","R&D Economic affairs","Economic affairs n.e.c.",NA,"Waste management","Waste water management","Pollution abatement","Protection of biodiversity and landscape","R&D Environmental protection","Environmental protection n.e.c.",NA,"Housing development","Community development","Water supply","Street lighting","R&D Housing and community amenities","Housing and community amenities n.e.c.",NA,"Medical products, appliances and equipment","Outpatient services","Hospital services","Public health services","R&D Health","Health n.e.c.",NA,"Recreational and sporting services","Cultural services","Broadcasting and publishing services","Religious and other community services","R&D Recreation, culture and religion","Recreation, culture and religion n.e.c.",NA,"Pre-primary and primary education","Secondary education","Post-secondary non-tertiary education","Tertiary education","Education not definable by level","Subsidiary services to education","R&D Education","Education n.e.c.",NA,"Sickness and disability","Old age","Survivors","Family and children","Unemployment","Housing","Social exclusion n.e.c.","R&D Social protection","Social protection n.e.c.")
cofog99=c("TOTAL","GF01","GF0101","GF0102","GF0103","GF0104","GF0105","GF0106","GF0107","GF0108","GF02","GF0201","GF0202","GF0203","GF0204","GF0205","GF03","GF0301","GF0302","GF0303","GF0304","GF0305","GF0306","GF04","GF0401","GF0402","GF0403","GF0404","GF0405","GF0406","GF0407","GF0408","GF0409","GF05","GF0501","GF0502","GF0503","GF0504","GF0505","GF0506","GF06", "GF0601","GF0602","GF0603","GF0604","GF0605","GF0606","GF07","GF0701","GF0702","GF0703","GF0704","GF0705","GF0706","GF08","GF0801","GF0802","GF0803","GF0804","GF0805","GF0806","GF09","GF0901","GF0902","GF0903","GF0904","GF0905","GF0906","GF0907","GF0908","GF10", "GF1001","GF1002","GF1003","GF1004","GF1005","GF1006","GF1007","GF1008","GF1009")
skt <- data.frame(razina1=rep("Rashodi",length(sektor1)),razina3=sektor1,razina4=sektor2,cofog99) %>% na.omit()
pom1 <- get_eurostat(id="gov_10a_exp") %>% filter(sector=="S13" & na_item %in% c("D1","D3","P2_D29_D5_D8","D4","D62_D632","D7","D9","OP5ANP")) %>% left_join(rzn,by="na_item") %>% inner_join(skt,by="cofog99") %>% mutate(datum = ceiling_date(time,"year")-1, razina5=razina4) %>% select(-time,-cofog99)
rm(skt,rzn,cofog99,sektor1,sektor2)
# 6.2. Porezni prihodi
load("razine_porezni_prihodi.Rda")
pom2 <- get_eurostat(id="gov_10a_taxag") %>% filter(sector=="S13" ) %>% left_join(rzn,by="na_item") %>% mutate(datum = ceiling_date(time,"year")-1) %>% select(-time) %>% na.omit()
rm(rzn)
# 6.3. Ostali prihodi
rzn <- data.frame(na_item=c("D39REC","D41REC","D42_TO_D45REC","D7REC","D8","D92_D99REC","P11_P12","P131"),razina1=rep("Ostali prihodi",8),razina2=c("Other subsidies on production","Property income","Property income","Other current transfers","Adj. for the change in pension entitlements","Capital transfers","Market output","Market output"),razina3=c("Other subsidies on production","Interest","Other property income","Other current transfers","Adj. for the change in pension entitlements","Other capital transfers and investment grants","Market output and output for own final use","Payments for non-market output"))
pom3 <- get_eurostat(id="gov_10a_main") %>% filter(sector=="S13" & na_item %in% c("D39REC","D41REC","D42_TO_D45REC","D7REC","D8","D92_D99REC","P11_P12","P131")) %>% left_join(rzn,by="na_item") %>% mutate(razina4=razina3, razina5=razina3) %>% mutate(datum = ceiling_date(time,"year")-1) %>% select(-time)
rm(rzn)

# 6.4. Sklapanje u jednu tablicu
javfin <- rbind(pom1,pom2,pom3) %>% left_join(reg,by="geo")
save(javfin,file = "javne_financije.Rda")
rm(pom1,pom2,pom3)

# 7. Platna bilanca ####

# 7.1. Uvoz i izvoz usluga se može razbiti po državama (zadnji podatak za 2018)
"bop_its6_tot"

# 7.2. Remittances se isto mogu razbiti po državama (zadnji podatak za 2019)
"bop_rem6" # ovdje su samo ukupne usluge
"bop_its6_det" # ovdje su razbijeni i po vrsti usluge

# 7.3. FDI po djelatnostima, po vrsti investicije, i po zemlji partneru
"bop_fdi6_pos" # ovo su stanja
"bop_fdi6_inc" # ovo je prihod po investicijama
"bop_fdi6_inc" # ovo je flow u godini dana
# 7.4. 

# 8. Turizam ####

# 8.1. Nights spent at tourist accommodation establishments

# Rezidenti vs nerezidenti
pom <- get_eurostat(id="tour_occ_nim") %>% filter(c_resid!="TOTAL" & nace_r2=="I551-I553" & unit=="NR" & geo=="HR")
ggplot(pom,aes(x=time,y=values,fill=c_resid)) + geom_area() + scale_y_continuous(labels = comma) + theme(legend.position = "top") + labs(x="",y="Broj noćenja") + boje_fill
# Vrste smještaja
pom <- get_eurostat(id="tour_occ_nim") %>% filter(c_resid=="TOTAL" & nace_r2 %in% c("I551","I552","I551") & unit=="NR" & geo=="HR") %>% mutate(godina=year(time),nace_r2=case_when(nace_r2=="I551"~"Hoteli",nace_r2=="I553"~"Kampovi",nace_r2=="I552"~"Ostalo")) %>% group_by(nace_r2,godina) %>% summarise(values=sum(values,na.rm=T))
ggplot(pom,aes(x=godina,y=values,fill=nace_r2)) + geom_col() + scale_y_continuous(labels = comma) + theme(legend.position = "top") + labs(x="",y="Broj noćenja") + boje_fill

# 8.2. Arrivals at tourist accommodation establishments

# Rezidenti vs nerezidenti
pom <- get_eurostat(id="tour_occ_arm") %>% filter(c_resid!="TOTAL" & nace_r2=="I551-I553" & unit=="NR" & geo=="HR")
ggplot(pom,aes(x=time,y=values,fill=c_resid)) + geom_area() + scale_y_continuous(labels = comma) + theme(legend.position = "top") + labs(x="",y="Broj dolazaka") + boje_fill
# Vrste smještaja
pom <- get_eurostat(id="tour_occ_arm") %>% filter(c_resid=="TOTAL" & nace_r2 %in% c("I551","I552","I551") & unit=="NR" & geo=="HR") %>% mutate(godina=year(time),nace_r2=case_when(nace_r2=="I551"~"Hoteli",nace_r2=="I553"~"Kampovi",nace_r2=="I552"~"Ostalo")) %>% group_by(nace_r2,godina) %>% summarise(values=sum(values,na.rm=T))
ggplot(pom,aes(x=godina,y=values,fill=nace_r2)) + geom_col() + scale_y_continuous(labels = comma) + theme(legend.position = "top") + labs(x="",y="Broj dolazaka") + boje_fill

# 8.3. Popunjenost kreveta u hotelima
pom <- get_eurostat(id="tour_occ_mnor") %>% filter(geo=="HR" & accommod=="BEDPL") %>% mutate(godina=year(time)) 
ggplot(pom,aes(x=time,y=values)) + geom_col()

# 8.4. Broj smještajnih jedinica
pom <- get_eurostat(id="tour_cap_nuts2") %>% filter(nace_r2 %in% c("I551","I552","I551") & unit=="NR" & geo%in% c("HR0","HR03","HR04") & accommod=="BEDPL") %>% mutate(nace_r2=case_when(nace_r2=="I551"~"Hoteli",nace_r2=="I553"~"Kampovi",nace_r2=="I552"~"Ostalo"),geo=case_when(geo=="HR0"~"Hrvatska",geo=="HR03"~"Jadran",geo=="HR04"~"Kontinent")) %>% group_by(geo,nace_r2,time) %>% summarise(values=sum(values,na.rm=T))
ggplot(pom,aes(x=time,y=values)) + geom_col() +facet_grid(geo~nace_r2,scales="free")+ scale_y_continuous(labels = comma) + theme(legend.position = "top") + labs(x="",y="Broj kreveta") + boje_fill

# 8.5. PArticipiranje u turizmu
pom <- get_eurostat(id="tour_dem_totot") %>% filter(unit=="PC_POP" & geo=="HR")
ggplot(pom,aes(x=time,y=values)) + geom_col() +facet_grid(partner~duration,scales="free")+ scale_y_continuous(labels = comma) + theme(legend.position = "top") + labs(x="",y="Broj kreveta") + boje_fill

# 8.6. Potrošnja u turizmu
pom <- get_eurostat(id="tour_dem_extot") %>% filter(unit=="THS_NAC" & geo=="HR" & duration=="N_GE1" & partner!="WORLD" & purpose=="TOTAL")
ggplot(pom,aes(x=time,y=values,fill=partner)) + geom_col() + scale_y_continuous(labels = comma) + theme(legend.position = "top") + labs(x="",y="000 HRK") + boje_fill


# 9. Inflacija ####
pom <- get_eurostat(id="prc_hicp_inw") %>% filter(coicop %in% c("CP01","CP02","CP03","CP04","CP05","CP06","CP07","CP08","CP09","CP10","CP11","CP12") & geo=="HR") %>% mutate(datum = ceiling_date(time,"year")-1) %>% mutate(dobro=case_when(coicop=="CP01"~"Hrana i piće",coicop=="CP02"~"Alkohol i cigare",coicop=="CP03"~"Odjeća i obuća",coicop=="CP04"~"Voda, struja i goriva",coicop=="CP05"~"Namještaj",coicop=="CP06"~"Zdravlje i edukacija",coicop=="CP07"~"Prijevoz",coicop=="CP08"~"Komunikacije",coicop=="CP09"~"Rekreacija i kultura",coicop=="CP10"~"Zdravlje i edukacija",coicop=="CP11"~"Restorani i hoteli",coicop=="CP12"~"Ostalo")) %>% mutate(values=values/10) %>% group_by(geo,datum,dobro) %>% summarise(values=sum(values,na.rm=T))
ggplot(pom,aes(x=datum,y=values,fill=dobro)) + geom_col() + boje_fill + theme(legend.position = "top",legend.title = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1)) + scale_x_date(breaks = "1 year",labels = date_format("%Y"))
kriza <- data.frame(begin=as.Date("2008-12-31"),end=as.Date("2010-12-31"))
ggplot(pom,aes(x=datum,y=values)) + geom_col() + geom_rect(data = kriza,aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf),inherit.aes = FALSE, fill = "red", alpha = 0.2) + facet_wrap(~dobro) + boje_fill + theme(legend.position = "top",legend.title = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1)) + scale_x_date(breaks = "1 year",labels = scales::date_format("%Y"))

# 10. Kratkoročni indikatori poslovnog okruženja ####

# 10.1. Proizvodnja industrije
pom <- get_eurostat("sts_inpr_m") %>% filter(nace_r2=="B-D" & s_adj=="SCA" & indic_bt=="PROD" & unit=="I15") %>% mutate(datum = ceiling_date(time,"month")-1) %>% left_join(reg,by="geo") %>% select(datum,geo,regija,values) %>% na.omit() %>% group_by(datum,regija) %>% filter(datum>="2000-01-01") %>% summarise(values=mean(values,na.rm = T))
ggplot(pom,aes(x=datum,y=values,col=regija)) + geom_line(size=2) + theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="",title="Industrijska proizvodnja",caption = paste("Zadnji datum: ",max(pom$datum))) + scale_x_date(breaks = "1 year",labels = scales::date_format("%y")) + boje_col

# 10.2. Turnover industrije
pom <- get_eurostat("sts_intv_m") %>% filter(nace_r2=="B_C" & s_adj=="SCA" & indic_bt=="TOVT" & unit=="I15") %>% mutate(datum = ceiling_date(time,"month")-1) %>% left_join(reg,by="geo") %>% select(datum,geo,regija,values) %>% na.omit() %>% group_by(datum,regija) %>% filter(datum>="2000-01-01") %>% summarise(values=mean(values,na.rm = T))
ggplot(pom,aes(x=datum,y=values,col=regija)) + geom_line(size=2) + theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="",title="Turnover industrije",caption = paste("Zadnji datum: ",max(pom$datum))) + scale_x_date(breaks = "1 year",labels = scales::date_format("%y")) + boje_col

# 10.3. Industrija - cijena proizvodnje
pom <- get_eurostat("sts_inpp_m") %>% filter(nace_r2=="B-D" & s_adj=="NSA" & indic_bt=="PRON" & unit=="I15") %>% mutate(datum = ceiling_date(time,"month")-1) %>% left_join(reg,by="geo") %>% select(datum,geo,regija,values) %>% na.omit() %>% group_by(datum,regija) %>% filter(datum>="2000-01-01") %>% summarise(values=mean(values,na.rm = T))
ggplot(pom,aes(x=datum,y=values,col=regija)) + geom_line(size=2) + theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="",title="Industrija - cijena proizvodnje",caption = paste("Zadnji datum: ",max(pom$datum))) + scale_x_date(breaks = "1 year",labels = scales::date_format("%y")) + boje_col

# 10.4. Industrija - cijene uvoza
pom <- get_eurostat("sts_inpi_m") %>% filter(nace_r2=="B-D" & s_adj=="NSA" & indic_bt=="IMPR" & unit=="I15") %>% mutate(datum = ceiling_date(time,"month")-1) %>% left_join(reg,by="geo") %>% select(datum,geo,regija,values) %>% na.omit() %>% group_by(datum,regija) %>% filter(datum>="2000-01-01") %>% summarise(values=mean(values,na.rm = T))
ggplot(pom,aes(x=datum,y=values,col=regija)) + geom_line(size=2) + theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="",title="Industrija - cijene uvoza",caption = paste("Zadnji datum: ",max(pom$datum))) + scale_x_date(breaks = "1 year",labels = scales::date_format("%y")) + boje_col

# 10.5. Građevina - proizvodnja
pom <- get_eurostat("sts_copr_m") %>% filter(nace_r2=="F" & s_adj=="SCA" & indic_bt=="PROD" & unit=="I15") %>% mutate(datum = ceiling_date(time,"month")-1) %>% left_join(reg,by="geo") %>% select(datum,geo,regija,values) %>% na.omit() %>% group_by(datum,regija) %>% filter(datum>="2000-01-01") %>% summarise(values=mean(values,na.rm = T))
ggplot(pom,aes(x=datum,y=values,col=regija)) + geom_line(size=2) + theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="",title="Građevina - proizvodnja",caption = paste("Zadnji datum: ",max(pom$datum))) + scale_x_date(breaks = "1 year",labels = scales::date_format("%y")) + boje_col

# 10.6. Trgovina - turnover
pom <- get_eurostat("sts_trtu_m") %>% filter(nace_r2=="G47" & s_adj=="SCA" & indic_bt=="TOVV" & unit=="I15") %>% mutate(datum = ceiling_date(time,"month")-1) %>% left_join(reg,by="geo") %>% select(datum,geo,regija,values) %>% na.omit() %>% group_by(datum,regija) %>% filter(datum>="2000-01-01") %>% summarise(values=mean(values,na.rm = T))
ggplot(pom,aes(x=datum,y=values,col=regija)) + geom_line(size=2) + theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="",title="Trgovina - turnover",caption = paste("Napomena: Samo maloprodaja","\nZadnji datum: ",max(pom$datum))) + scale_x_date(breaks = "1 year",labels = scales::date_format("%y")) + boje_col

# 10.7. Usluge - turnover
pom <- get_eurostat("sts_setu_m") %>% filter(nace_r2=="H-N_STS" & s_adj=="SCA" & indic_bt=="TOVT" & unit=="I15") %>% mutate(datum = ceiling_date(time,"month")-1) %>% left_join(reg,by="geo") %>% select(datum,geo,regija,values) %>% na.omit() %>% group_by(datum,regija) %>% filter(datum>="2000-01-01") %>% summarise(values=mean(values,na.rm = T))
ggplot(pom,aes(x=datum,y=values,col=regija)) + geom_line(size=2) + theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="",title="Usluge - turnover",caption = paste("Napomena: Djelatnosti H do N.","\nZadnji datum: ",max(pom$datum))) + scale_x_date(breaks = "1 year",labels = scales::date_format("%y")) + boje_col

# 10.71. Prijevoz - turnover
pom <- get_eurostat("sts_setu_m") %>% filter(nace_r2=="H" & s_adj=="SCA" & indic_bt=="TOVT" & unit=="I15") %>% mutate(datum = ceiling_date(time,"month")-1) %>% left_join(reg,by="geo") %>% select(datum,geo,regija,values) %>% na.omit() %>% group_by(datum,regija) %>% filter(datum>="2000-01-01") %>% summarise(values=mean(values,na.rm = T))
ggplot(pom,aes(x=datum,y=values,col=regija)) + geom_line(size=2) + theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="",title="Prijevoz - turnover",caption = paste("Napomena: Djelatnost H.","\nZadnji datum: ",max(pom$datum))) + scale_x_date(breaks = "1 year",labels = scales::date_format("%y")) + boje_col

# 10.72. Smještaj - turnover
pom <- get_eurostat("sts_setu_m") %>% filter(nace_r2=="I" & s_adj=="SCA" & indic_bt=="TOVT" & unit=="I15") %>% mutate(datum = ceiling_date(time,"month")-1) %>% left_join(reg,by="geo") %>% select(datum,geo,regija,values) %>% na.omit() %>% group_by(datum,regija) %>% filter(datum>="2000-01-01") %>% summarise(values=mean(values,na.rm = T))
ggplot(pom,aes(x=datum,y=values,col=regija)) + geom_line(size=2) + theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="",title="Smještaj - turnover",caption = paste("Napomena: Djelatnost I.","\nZadnji datum: ",max(pom$datum))) + scale_x_date(breaks = "1 year",labels = scales::date_format("%y")) + boje_col

# 10.8. Usluge - cijena proizvodnje
pom <- get_eurostat("sts_sepp_q") %>% filter(nace_r2=="H494-N812_STS" & s_adj=="NSA" & indic_bt=="PRON" & unit=="I15") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% left_join(reg,by="geo") %>% select(datum,geo,regija,values) %>% na.omit() %>% group_by(datum,regija) %>% filter(datum>="2000-01-01") %>% summarise(values=mean(values,na.rm = T))
ggplot(pom,aes(x=datum,y=values,col=regija)) + geom_line(size=2) + theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="",title="Usluge - cijena proizvodnje",caption = paste("Zadnji datum: ",max(pom$datum))) + scale_x_date(breaks = "1 year",labels = scales::date_format("%y")) + boje_col

# 11. Prinosi i kstope ####

# 11.1. kstope na stanja (HH + NFC)
pom <- get_data(key = "MIR.M..B.A20.A.R.A..EUR.O") %>% mutate(sektor=case_when(bs_count_sector=="2240"~"NFC",bs_count_sector=="2250"~"HH")) %>% mutate(datum = make_date(ifelse(substr(obstime,6,7)=="12",as.numeric(substr(obstime,1,4))+1,as.numeric(substr(obstime,1,4))), ifelse(substr(obstime,6,7)=="12",1,as.numeric(substr(obstime,6,7))+1), 1)-1) %>% select(datum,geo=ref_area,sektor,kstopa=obsvalue) 
ggplot(pom,aes(x=datum,y=kstopa)) + geom_line() + facet_wrap(~geo, scales="free")

# 11.2. Yieldovi na dugoročne državne obveznice - EMU convergence criterion bond yields ####
pom2 <- get_eurostat(id="irt_lt_mcby_m") %>% select(-int_rt,kstopa=values) %>% mutate(datum = ceiling_date(time,"month")-1,sektor="govt_yld") %>% filter(datum>"2002-12-31") %>% select(datum,geo,sektor,kstopa) %>% na.omit()
pom <- rbind(pom1,pom2)
ggplot(pom,aes(x=datum,y=kstopa,col=sektor)) + geom_line(size=1.2) + facet_wrap(~geo, scales="free") + boje_col


# Model marže na kredite HH&NFC
# marža
pom1 <- get_data(key = "RAI.M..LMGOLNFCH..MIR.Z") %>% mutate(datum = make_date(ifelse(substr(obstime,6,7)=="12",as.numeric(substr(obstime,1,4))+1,as.numeric(substr(obstime,1,4))), ifelse(substr(obstime,6,7)=="12",1,as.numeric(substr(obstime,6,7))+1), 1)-1) %>% select(datum,geo=ref_area,marze_HH_NFC=obsvalue) %>% group_by(datum,geo) %>% summarise(marze_HH_NFC=mean(marze_HH_NFC,na.rm=T)) %>% mutate(marze_HH_NFC=ifelse(marze_HH_NFC==1,NA,marze_HH_NFC))
# 
ggplot(pom,aes(x=datum,y=marze_HH_NFC)) + geom_line() + facet_wrap(~geo, scales="free")

# 12. Tecaj ####
pom <- get_data("EXR.Q.HRK.EUR.SP00.A") %>% select(obstime,tecaj_eur=obsvalue) %>% mutate(datum = make_date(ifelse(substr(obstime,7,7)=="4",as.numeric(substr(obstime,1,4))+1,as.numeric(substr(obstime,1,4))), ifelse(substr(obstime,7,7)=="4",1,as.numeric(substr(obstime,7,7))*3+1), 1)-1) %>% select(-obstime)

# 13. Prinosi ####
# 1.1. Yieldovi na dugoročne državne obveznice - EMU convergence criterion bond yields ####
pom <- get_eurostat(id="irt_lt_mcby_m") %>% filter(geo=="HR") %>% mutate(datum = ceiling_date(time,"month")-1) %>% select(datum,geo,yld=values)
ggplot(pom,aes(x=datum,y=yld))+geom_line()
