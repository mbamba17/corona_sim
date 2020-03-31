library(readxl)
library(zoo)
library(tidyverse)
library(lubridate)

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
poduzeca=rbind(temp1,temp2,temp3,temp4,temp5)
# sklapanje sa šiframa
poduzeca <- full_join(sfr,poduzeca,by="djelatnost")
rm(temp1,temp2,temp3,temp4,temp5,sfr,i,pom1,pom2,pom3)