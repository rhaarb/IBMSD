library(haven)
library(dplyr)
library(tidyverse)

# Health data - first book contains relevant info for blood pressure
bus_1.0 <- read_dta(file="~/IFLS4/hh07_bus_dta/bus1_0.dta")
bus_1.1 <- read_dta(file="~/IFLS4/hh07_bus_dta/bus1_1.dta")
bus_1.2 <- read_dta(file="~/IFLS4/hh07_bus_dta/bus1_2.dta")
bus_1.3 <- read_dta(file="~/IFLS4/hh07_bus_dta/bus1_3.dta")
bus_1.4 <- read_dta(file="~/IFLS4/hh07_bus_dta/bus1_4.dta")

# Health self assessment
cd3 <- read_dta(file="~/IFLS4/hh07_b3b_dta/b3b_cd3.dta") 

# Education data
dl1 <- read_dta(file="~/IFLS4/hh07_b3a_dta/b3a_dl1.dta") 
dl2 <- read_dta(file="~/IFLS4/hh07_b3a_dta/b3a_dl2.dta") 

# Smoking
km <- read_dta(file="~/IFLS4/hh07_b3b_dta/b3b_km.dta") 

# Income / economic self-assessment
sw <- read_dta(file="~/IFLS4/hh07_b3a_dta/b3a_sw.dta") 
tk2 <- read_dta(file="~/IFLS4/hh07_b3a_dta/b3a_tk2.dta") 

# Location
sc <- read_dta(file="~/IFLS4/hh07_bk_dta/bk_sc.dta") 

# Weights
ptrack <- read_dta(file="~/IFLS4/hh07_trk_dta/ptrack.dta") 


# Extract household id and interviewer id
bus_1.0 <- bus_1.0 %>% select(hhid07_9, idivwr)

# merge first, second and third blood pressure measurement
# first create unique IDs for individuals
bus_1.1 <- bus_1.1 %>% mutate(new.id=paste0(hhid07_9,pid07))
bus_1.2 <- bus_1.2 %>% mutate(new.id=paste0(hhid07_9,pid07))
bus_1.4 <- bus_1.4 %>% mutate(new.id=paste0(hhid07_9,pid07))

# merge
comb <- bus_1.1 %>%
  left_join(bus_1.2, by = "new.id")

comb <- comb %>%
  left_join(bus_1.4, by = "new.id")

comb <- comb %>%
  left_join(bus_1.0, by = "hhid07_9")

comb <- comb %>%
  left_join(dl1, by = "pidlink")

comb <- comb %>%
  left_join(km, by = "pidlink")

comb <- comb %>%
  left_join(sw, by = "pidlink")

comb <- comb %>%
  left_join(tk2, by = "pidlink")

colnames(comb)[which(colnames(comb)=="hhid07_9.x")] <- "hhid07_9"

comb <- comb %>%
  left_join(sc, by = "hhid07_9")

cd3 <- cd3 %>% filter(cdtype=="A")

comb <- comb %>%
  left_join(cd3, by = "pidlink")

comb <- comb %>%
  left_join(ptrack, by = "pidlink")

# Select variables
comb_2 <- comb %>% select(
  
  # sex
  us01,
  
  # age
  us03,
  
  # weight
  us06,
  
  # height,
  us04,
  
  # systolic blood pressure first measurement
  us07a1,
  
  # diastolic blood pressure first measurement
  us07a2,
  
  # systolic blood pressure second measurement
  us07b1,
  
  # diastolic blood pressure second measurement
  us07b2,
  
  # systolic blood pressure second measurement
  us07c1,
  
  # diastolic blood pressure second measurement
  us07c2,
  
  # are you taking medicine for hypertension
  us18ab,
  
  # interviewer ID
  idivwr,
  
  # household ID
  hhid07_9,
  
  # personal ID
  pidlink,
  
  # highest level of education attended
  dl06,
  
  # Do you still smoke or have you quit
  km04,
  
  # Smoking
  km01a,
  
  # Have a doctor/paramedic ever told you that you had [...] ?
  cd05,
  
  # Type of Chronic Disease (pre-selected so that only "A: hypertension" remains)
  cdtype,
  
  # In order to deal with [..] are you currently,
  cd09,
  
  # economic self assessment, proxy for income? 
  sw01,
  
  # approx. income last month
  tk25a1,
  
  # income, net profit (self-employed)
  tk26a1,
  
  # province
  sc010707,
  
  # kab (?), next finer level of location after province
  sc020707,
  
  # kec (?), next finer level of location after kab
  sc030707,
  
  # rural vs. urban
  sc05,
  
  # weight w/ attrition,
  pwt07xa,
  
  # weight w/o attrition
  pwt07x_

  )

comb_2 <- comb_2 %>%
  mutate(
    IFLS=4,
    province=as.factor(sc010707),
    municipality=as.factor(paste0(sc010707,sc020707)),
    subdistrict=as.factor(paste0(sc010707,sc020707,sc030707)),
    ruralurban=as.factor(sc05),
    p.id=as_factor(pidlink),
    hh.id=as_factor(hhid07_9),
    int.id=as_factor(idivwr),
    sex=ifelse(is.na(us01),NA,ifelse(us01==1,0,1)), # 0=male, 1=female
    age=ifelse(us03>110,NA,us03),
    age.sq=age^2,
    height=us04/100,
    weight=us06,
    bmi=weight/(height^2),
    bmi=ifelse(bmi>100,NA,bmi),
    bmi.sq=bmi^2,
    # waist.crcmf=us06a,
    # hip.crcmf=us06b,
    edu=ifelse(as.numeric(dl06) %in% c(2,11,72), "Elementary",
               ifelse(as.numeric(dl06) %in% c(3,4,5,6,12,15,14,73,74), "Secondary",
                      ifelse(as.numeric(dl06) %in% c(13,60,61,62,63), "Tertiary", NA))),
    edu=fct_relevel(edu,"Elementary","Secondary","Tertiary"),
    sbp1=us07a1,
    dbp1=us07a2,
    sbp2=us07b1,
    dbp2=us07b2,
    sbp3=us07c1,
    dbp3=us07c2,
    sbp.main=(sbp2+sbp3)/2,
    sbp.main=ifelse(sbp.main>260,NA,sbp.main),
    dbp.main=(dbp2+dbp3)/2,
    income=ifelse(is.na(tk25a1), tk26a1, tk25a1),
    income=ifelse(income>300000000,NA,income),
    income.scaled=scale(income),
    income.log=log(income+1),
    smoker=ifelse(km01a==3,0,ifelse(km04==1,1,ifelse(km04==3,0,NA))),
    hypertension.diag=ifelse(cd05==1,1,ifelse(cd05==3,0,NA)),
    hypertension.med=ifelse(cd09==1,1,ifelse(cd09==3,0,NA)),
    health_wght_w_attr=pwt07xa,
    health_wght_wout_attr=pwt07x_
  ) %>% select(
    IFLS, province, municipality, subdistrict, ruralurban, p.id, hh.id, int.id, sex, age, age.sq, bmi, bmi.sq, edu, sbp1, sbp2, sbp3, dbp1, dbp2, dbp3, sbp.main,
    dbp.main, income.scaled, income.log, smoker,
    hypertension.diag,hypertension.med,health_wght_w_attr,health_wght_wout_attr
  ) %>% filter(
    !is.na(sbp2), !is.na(sbp3)
  ) %>% distinct()

save(comb_2, file="~/ifls4.Rdata")


