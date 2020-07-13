library(tidyverse)
library(lubridate)
library(readxl)
library(scales)

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

# Zemlje članice EU
reg = data.frame(geo=c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE","UK"),ctry=c("AUT","BEL","BLG","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT","ROU","SVK","SVN","ESP","SWE","GBR"),country=c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Rep.","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom"),regija=c("Ostale zemlje EU","Ostale zemlje EU","Zemlje SIE","HR","Ostale zemlje EU","Zemlje SIE","Ostale zemlje EU","Zemlje SIE","Ostale zemlje EU","Ostale zemlje EU","Ostale zemlje EU","Ostale zemlje EU","Zemlje SIE","Ostale zemlje EU","Ostale zemlje EU","Zemlje SIE","Zemlje SIE","Ostale zemlje EU","Ostale zemlje EU","Ostale zemlje EU","Zemlje SIE","Ostale zemlje EU","Zemlje SIE","Zemlje SIE","Zemlje SIE","Ostale zemlje EU","Ostale zemlje EU","Ostale zemlje EU"))

# 1. Učitavanje podataka ####

load("Z:/DSR/DWH/finpodaci.Rda")
load("Z:/DSR/DWH/nav_opce.Rda")
load("Z:/DSR/Financijski_racuni/FinRacuni.Rda")
load("Z:/DSR/DWH/ZSE_trans.Rda")
load("Z:/DSR/DWH/imovina_S2.Rda")
load("Z:/DSR/DWH/NAV.Rda")

# 2. Izračun prinosa za fondove ####

obveznice <- nav %>% filter(izvjestaj=="NAV" & razina2=="Dugoročni dužnički vrijednosni papiri") %>% select(-sektor,-djelatnost1,-djelatnost2,-djelatnost3,-djelatnost4,-djelatnost5,-naziv,-sektor_kod,-sektor_2010,-obrazac,-izvjestaj,-oib,-mb,-kolicina)
temp <- read_excel("obveznice.xlsx",sheet = "vrijednosti",skip = 1,na = c("#N/A Field Not Applicable","#N/A Invalid Security")) %>% select(isin,kupon=CPN,frekvencija=CPN_FREQ,datum_izdanja=SECURITY_PRICING_DATE,datum_dospijeca=MATURITY,kolicina_izdanje=AMT_ISSUED) %>% mutate(datum_izdanja=as.Date(datum_izdanja,"%d.%m.%Y."),datum_dospijeca=as.Date(datum_dospijeca,"%d.%m.%Y."),kupon=as.numeric(kupon)) %>% filter(!is.na(kupon)) %>% mutate(frekvencija=ifelse(is.na(frekvencija),2,frekvencija))
obveznice <- obveznice %>% inner_join(temp,by="isin") %>% mutate(dospijece=datum_dospijeca) %>% select(-datum_dospijeca)

# 3. Izračun prinosa do dospijeća
pom <- obveznice %>% mutate(T=(dospijece-datum)/365,M=nominala_u_valuti,n=frekvencija,P=jed_cijena_u_valuti,C=(kupon/100)*M/n) %>% mutate(T=as.numeric(T),ytm=NA,P=P*M)
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
# popunjavanje praznih polja (tamo gdje nije bilo moguće izračunati yield) <- tu tražimo negativan yideld
for (i in 1:nrow(pom)){
  if(is.na(pom$ytm[i])){
    tryCatch({
      f_cijena <- function(y) pom$C[i]/(y/pom$n[i])*(1-1/(1+y/pom$n[i])^(pom$n[i]*pom$T[i])) + pom$M[i]/(1+y/pom$n[i])^(pom$n[i]*pom$T[i]) - pom$P[i]
      pom$ytm[i] <- uniroot(f_cijena, interval = c(-0.1, -0.0000000001), tol = 0.0000000001)$root
    }, error=function(e){})
  }
}
# popunjavanje praznih polja (tamo gdje nije bilo moguće izračunati yield) <- tu tražimo zadnji prethodni datum i onda uzimamo njegov yield
for (i in 1:nrow(pom)){
  if(is.na(pom$ytm[i])){
    tryCatch({
      procjena <- pom %>% filter(datum<pom$datum[i] & isin==pom$isin[i]) %>% arrange(datum)
      pom$ytm[i] <- procjena$ytm[nrow(procjena)]
    }, error=function(e){})
  }
}
#bla <- pom %>% rowid_to_column() %>% filter(is.na(ytm))
obveznice <- pom
rm(i,procjena,f_cijena,temp,pom)

# 4. Crtanje prinosa ####
pom <- obveznice %>% filter(!is.na(ytm) & datum>="2014-01-31" & razina3 %in% c("Državne, središnjih banaka i javnih međunarodnih tijela","Municipalne","Državne, središnjih banaka i međunarodnih organizacija") & drzava %in% c("US","HR","DE")) %>% group_by(datum,drzava) %>% summarise(ytm=weighted.mean(x=ytm,w=iznos))
pom1 <- obveznice %>% filter(datum>="2014-01-31" & drzava=="HR" & razina3 %in% c("Državne, središnjih banaka i javnih međunarodnih tijela","Municipalne","Državne, središnjih banaka i međunarodnih organizacija")) %>% group_by(datum,subjekt) %>% summarise(ytm=weighted.mean(x=ytm,w=iznos)) %>% group_by(datum) %>% summarise(p10=quantile(ytm,probs = 0.1,na.rm = T),p25=quantile(ytm,probs = 0.25,na.rm = T),p75=quantile(ytm,probs = 0.75,na.rm = T),p90=quantile(ytm,probs = 0.9,na.rm = T))
ggplot(pom,aes(x=datum,y=ytm,col=drzava)) + boje_col + geom_ribbon(data=pom1,inherit.aes = F,aes(x=datum,ymin=p10,ymax=p90,fill="#d9455f"),alpha=0.3,show.legend = F) + geom_ribbon(data=pom1,inherit.aes = F,aes(x=datum,ymin=p25,ymax=p75,fill="#d9455f"),alpha=0.4,show.legend = F) + geom_line(size=1.1) + scale_y_continuous(labels = percent)
