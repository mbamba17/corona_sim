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
temp <- read_excel(path = "place.xlsx",sheet="izvoz_r")
#v1
mdl <- lm(dwage~infl+unemp+dprod,data = temp)
summary(mdl)
plot(mdl)
pom <- data.frame(mdl$model,reziduali=mdl$residuals) %>% mutate(dwage_proc=dwage-reziduali)
skopiraj(pom)
