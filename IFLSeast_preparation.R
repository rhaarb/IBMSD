library(haven)
library(dplyr)
library(tidyverse)

# Health data - first book contains relevant info for blood pressure
bus_1.0 <- read_dta(file="~/IFLSeast/hhe12_bus/BUS_COV.dta")
bus_1.1 <- read_dta(file="~/IFLSeast/hhe12_bus/BUS_US1.dta")
bus_1.2 <- read_dta(file="~/IFLSeast/hhe12_bus/BUS_US2.dta")
bus_1.3 <- read_dta(file="~/IFLSeast/hhe12_bus/BUS_US3.dta")
bus_1.4 <- read_dta(file="~/IFLSeast/hhe12_bus/BUS_US4.dta")
bus_1.5 <- read_dta(file="~/IFLSeast/hhe12_bus/BUS_US5.dta")

# Health self assessment
# cd1 <- read_dta(file="~IFLSeast/hhe12_b3b/B3B_CD1.dta")
# cd2 <- read_dta(file="~IFLSeast/hhe12_b3b/B3B_CD2.dta")
cd3 <- read_dta(file="~/IFLSeast/hhe12_b3b/B3B_CD3.dta")

# Education data
dl1 <- read_dta(file="~IFLSeast/hhe12_b3a/B3A_DL1.dta")
dl2 <- read_dta(file="~IFLSeast/hhe12_b3a/B3A_DL2.dta")

# Smoking
km <- read_dta(file="~IFLSeast/hhe12_b3b/B3B_KM1.dta")

# Income / economic self-assessment
sw <- read_dta(file="~IFLSeast/hhe12_b3a/B3A_SW1.dta") 
tk2 <- read_dta(file="~IFLSeast/hhe12_b3a/B3A_TK2.dta") 

# Location
sc <- read_dta(file="~IFLSeast/hhe12_bk/BK_SC1.dta") 

# Weights
ptrack <- read_dta(file="~IFLSeast/hhe12_track/ptrack.dta")


# Extract household id and interviewer id
bus_1.0 <- bus_1.0 %>% select(hhid12, idivwus)

# merge first, second and third blood pressure measurement
# first create unique IDs for individuals
bus_1.1 <- bus_1.1 %>% mutate(new.id=paste0(hhid12,usar00))
bus_1.2 <- bus_1.2 %>% mutate(new.id=paste0(hhid12,usar00b))
bus_1.4 <- bus_1.4 %>% mutate(new.id=paste0(hhid12,usar00d))
bus_1.5 <- bus_1.5 %>% mutate(new.id=paste0(hhid12,usar00e))

km <- km %>% mutate(new.id=paste0(hhid12,pid12))
dl1 <- dl1 %>% mutate(new.id=paste0(hhid12,pid12))
dl2 <- dl2 %>% mutate(new.id=paste0(hhid12,pid12))
sw <- sw %>% mutate(new.id=paste0(hhid12,pid12))
tk2 <- tk2 %>% mutate(new.id=paste0(hhid12,pid12))
cd3 <- cd3 %>% mutate(new.id=paste0(hhid12,pid12))
ptrack <- ptrack %>% mutate(new.id=paste0(hhid12,substring(ptrack$pid12, 2)))

# merge
comb <- bus_1.1 %>%
  left_join(bus_1.2, by = "new.id")

comb <- comb %>%
  left_join(bus_1.4, by = "new.id")

comb <- comb %>%
  left_join(bus_1.5, by = "new.id")

colnames(comb)[which(colnames(comb)=="hhid12.x")] <- "hhid12"

comb <- comb %>%
  left_join(bus_1.0, by = "hhid12")

# Education data
comb <- comb %>%
  left_join(dl1, by = "new.id")

comb <- comb %>%
  left_join(dl2, by = "new.id")

# Smoking
comb <- comb %>%
  left_join(km, by = "new.id")

# Income / economic self-assessment
comb <- comb %>%
  left_join(sw, by = "new.id")

comb <- comb %>%
  left_join(tk2, by = "new.id")

# Health self assessment
cd3 <- cd3 %>% filter(cd3btype=="A")

comb <- comb %>%
  left_join(cd3, by = "new.id")

# Weights
comb <- comb %>%
  left_join(ptrack, by = "new.id")

# Location
colnames(comb)[which(colnames(comb)=="hhid12.x")] <- "hhid12"

comb <- comb %>%
  left_join(sc, by = "hhid12")

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
  idivwus,
  
  # household ID
  hhid12,
  
  # personal ID
  new.id,
  
  # highest level of education attended
  dl06,
  
  # Do you still smoke or have you quit
  km04,
  
  # Smoking
  km01a,
  
  # Have a doctor/paramedic ever told you that you had [...] ?
  cd053b,
  
  # Type of Chronic Disease (pre-selected so that only "A: hypertension" remains)
  cd3btype,
  
  # In order to deal with [..] are you currently,
  cd093b,
  
  # economic self assessment, proxy for income? 
  sw01,
  
  # approx. income last month
  tk25a1,
  
  # income, net profit (self-employed)
  tk26a1,
  
  # province
  sc01n,
  
  # kab (?), next finer level of location after province
  sc02n,
  
  # kec (?), next finer level of location after kab
  sc03n,
  
  # rural vs. urban
  sc05,
  
  # weight w/ attrition,
  # pwt07xa,
  
  # weight w/o attrition
  pwtus
  
)

comb_2 <- comb_2 %>%
  mutate(
    IFLS="east",
    province=as.factor(sc01n),
    municipality=as.factor(paste0(sc01n,sc02n)),
    subdistrict=as.factor(paste0(sc01n,sc02n,sc03n)),
    ruralurban=as.factor(sc05),
    p.id=as_factor(new.id),
    hh.id=as_factor(hhid12),
    int.id=as_factor(idivwus),
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
    hypertension.diag=ifelse(cd053b==1,1,ifelse(cd053b==3,0,NA)),
    hypertension.med=ifelse(cd093b==1,1,ifelse(cd093b==3,0,NA)),
    # health_wght_w_attr=pwt07xa,
    health_wght_wout_attr=pwtus
  ) %>% select(
    IFLS, province, municipality, subdistrict, ruralurban, p.id, hh.id, int.id, sex, age, age.sq, bmi, bmi.sq, edu, sbp1, sbp2, sbp3, dbp1, dbp2, dbp3, sbp.main,
    dbp.main, income.scaled, income.log, smoker,
    hypertension.diag,hypertension.med,health_wght_wout_attr
  ) %>% filter(
    !is.na(sbp2), !is.na(sbp3)
  ) %>% distinct()

save(comb_2, file="~/iflseast.Rdata")
