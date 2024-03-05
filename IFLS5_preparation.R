library(haven)
library(dplyr)
library(tidyverse)

cov <- read_dta(file="~/hh14_bus_dta/bus_cov.dta")
us <- read_dta(file="~/hh14_bus_dta/bus_us.dta")
tk2 <- read_dta(file="~/hh14_b3a_dta/b3a_tk2.dta")
cd3 <- read_dta(file="~/hh14_b3b_dta/b3b_cd3.dta")
km <- read_dta(file="~/hh14_b3b_dta/b3b_km.dta")
sc <- read_dta(file="~/hh14_bk_dta/bk_sc1.dta")
dl <- read_dta(file="~/hh14_b3a_dta/b3a_dl1.dta")
ptrack <- read_dta(file="~/hh14_trk_dta/ptrack.dta")

# Link interviewer IDs to main data set
comb <- us %>%
        left_join(sc, by = "hhid14_9")

comb <- comb %>%
          left_join(cov, by = "pidlink")

comb <- comb %>%
            left_join(tk2, by ="pidlink")

cd3 <- filter(cd3, cdtype=="A")

comb <- comb %>%
            left_join(cd3, by ="pidlink")

comb <- comb %>%
            left_join(km, by ="pidlink")

comb <- comb %>%
            left_join(dl, by ="pidlink")

comb <- comb %>%
            left_join(ptrack, by ="pidlink")


# Block 1: blood pressure
comb_2 <- comb %>%
              select(
                # province,
                sc01_14_14,
                # municipality,
                sc02_14_14,
                # subdistrict,
                sc03_14_14,
                # rural / urban,
                sc05,
                # patient ID,
                pidlink, 
                # interviewer ID,
                idivwr,
                # sex,
                us01, 
                # age,
                us03,
                #height,
                us04,
                # hh ID,
                hhid14_9.x,
                # weight,
                us06,
                # waist circumfence,
                us06a,
                # hip cirumfence,
                us06b,
                # systolic blood pressure measurement 1,
                us07a1,
                # diastolic blood pressure measurement 1,
                us07a2,
                # systolic blood pressure measurement 2,
                us07b1,
                # diastolic blood pressure measurement 2,
                us07b2,
                # systolic blood pressure measurement 3,
                us07c1,
                # diastolic blood pressure measurement 3,
                us07c2,
                # takes anti-hypertension medication,
                us18ab,
                # Approximately what was your salary/wage during the last month? Job1,
                tk25a1,
                # Approximately what was your salary/wage during the last year?  Job1
                tk25a2,
                # Approximately how much net profit did you gain last month? Job1 (SELF-EMPLOYED),
                tk26a1,
                # Have you ever had [tobacco habit]?
                km01a,
                # Do you still have the habit or have you totally quit?
                km04,
                # Have a doctor/paramedic ever told you that you had [...] ?
                cd05,
                # Type of Chronic Disease (pre-selected so that only "A: hypertension" remains)
                cdtype,
                # In order to deal with [..] are you currently,
                cd09,
                # Highest level of education attended,
                dl06,
                #  What is the highest grade completed at that school?,
                dl07,
                # IFLS5 cross-section US (health data book) weight w/ attrition,
                pwt14usxa,
                # IFLS5 cross-section US (health data book) weight w/o attrition
                pwt14usx_
                )

comb_2 <- comb_2 %>%
                mutate(
                  IFLS=5,
                  year="2014-2015",
                  province=as.factor(sc01_14_14),
                  municipality=as.factor(paste0(sc01_14_14,sc02_14_14)),
                  subdistrict=as.factor(paste0(sc01_14_14,sc02_14_14,sc03_14_14)),
                  ruralurban=as.factor(sc05),
                  p.id=as_factor(pidlink),
                  hh.id=as_factor(hhid14_9.x),
                  int.id=as_factor(idivwr),
                  sex=ifelse(us01==1,0,1),
                  age=ifelse(us03>110,NA,us03),
                  age.sq=age^2,
                  height=us04/100,
                  weight=us06,
                  bmi=weight/(height^2),
                  bmi=ifelse(bmi>100,NA,bmi),
                  bmi.sq=bmi^2,
                  waist.crcmf=us06a,
                  hip.crcmf=us06b,
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
                  dbp.main=(dbp2+dbp3)/2,
                  income=ifelse(is.na(tk25a1), tk26a1, tk25a1),
                  income=ifelse(income>300000000,NA,income),
                  income.scaled=scale(income),
                  income.log=log(income+1),
                  smoker=ifelse(km01a==3,0,ifelse(km04==1,1,ifelse(km04==3,0,NA))),
                  hypertension.diag=ifelse(cd05==1,1,ifelse(cd05==3,0,NA)),
                  hypertension.med=ifelse(cd09==1,1,ifelse(cd09==3,0,NA)),
                  health_wght_w_attr=pwt14usxa,
                  health_wght_wout_attr=pwt14usx_
                ) %>% select(
                  IFLS, year, province, municipality, subdistrict, ruralurban, p.id, hh.id, int.id, sex, age, age.sq, bmi, bmi.sq, edu, sbp1, sbp2, sbp3, dbp1, dbp2, dbp3, sbp.main,
                  dbp.main,
                  income.scaled, income.log, smoker,
                  hypertension.diag,hypertension.med,health_wght_w_attr,health_wght_wout_attr
                ) %>% filter(
                  !is.na(sbp2), !is.na(sbp3)
                )

save(comb_2, file="~/ifls5.Rdata")

