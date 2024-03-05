capture log close

// Create LASI analytic file

// Note! This includes restricted data, and therefore cannot be run
// without access to those.

clear all
set more off
set maxvar 32767
set matsize 11000
set varabbrev off
capture program drop _all

log using "make_lasi_data.log", text replace
set linesize 254

// ----------------------------------------------------------------------
// Part 1: Process Harmonized LASI data

// Harmonized LASI data, version A.2 (Oct. 2021)
local HLASI "./data/H_LASI_a2.dta"

// Identifiers, interview status, and other technical variables
local id prim_key hhid inw1 inw1pm r1wtresp
// Demographics
local demo ragender r1agey raeduc_l r1mstat hh1rural hh1state r1caste
// Health, health behaviors, and related variables
local health r1hibpe r1rxhibp r1smokev r1smoken r1smokef r1otbccv
// Economic status
local econ hh1itot hh1cperc r1worka
// Physical measures
local phys r1systo* r1diasto* r1bpcomp r1bldpos r1bparm r1bpcompl r1bpact30 ///
    r1mheight r1mweight r1mbmi r1mbmicat r1htcomp r1wtcomp r1htcompl r1wtcompl

local allharm `id' `demo' `health' `econ' `phys'

use `allharm' using "`HLASI'", clear

recode raeduc_l (0=0 "0.No schooling") (1=1 "1.Less than primary") ///
    (2/6=2 "2.Primary or secondary") (7/9=3 "3.Tertiary"), generate(raeduc3)
label variable raeduc3 "Education (0-3 cat)"

generate double loginc = ln(hh1itot) 
generate double logpce = ln(hh1cperc)
label variable loginc "Log hh income"
label variable logpce "Log per capita consumption"

generate byte smoker = r1smokev
replace smoker = 2 if (r1smoken == 1)
label variable smoker "Smoker"
label define smoker 0 "0.Never smoked" 1 "1.Ex-smoker" 2 "2.Current smoker"
label values smoker smoker

generate byte incsign = .
replace incsign = 1 if (hh1itot < 0  & !missing(hh1itot))
replace incsign = 2 if (hh1itot == 0 & !missing(hh1itot))
replace incsign = 3 if (hh1itot > 0  & !missing(hh1itot))
label variable incsign "Sign of hh income"
label define incsign 1 "Negative" 2 "Zero" 3 "Positive"
label values incsign incsign

compress
tempfile temphlasi
save "`temphlasi'", replace

// -------------------------------------------------------------------
// Part 2: process restricted data

// Districts and SSUs

import excel "./data/2022-06/LASI_SSUlist_allPhase_wave1.xlsx" ///
    , sheet("lasi_psus") firstrow case(lower) clear

rename distrcitid districtid

keep ssuid districtid

compress
tempfile tempdistrict
save "`tempdistrict'", replace

// Interviewer ID and SSU

use hhid ssuid urid using "./data/2022-06/lasi_cv2_all.dta"
merge m:1 ssuid using "`tempdistrict'"
drop if (_merge == 2)
drop _merge

compress
tempfile temprestricted
save "`temprestricted'", replace

// -------------------------------------------------------------------
// Part 3: Merge Harmonized LASI extract with restricted data

use "`temphlasi'", clear
merge m:1 hhid using "`temprestricted'"
drop if (_merge == 2)
rename _merge mergehc

generate byte asamp = (r1agey >= 45 & !missing(r1agey) & !missing(r1systo) & mergehc == 3)
label variable asamp "Analytic sample"

compress
save "lasi03.dta", replace

tabulate asamp, missing
summarize _all
summarize _all if (asamp)

// -------------------------------------------------------------------
// Part 4: Strip restricted data again, for computing cross-study
// descriptives in R

drop districtid ssuid urid mergehc
keep if (asamp)
drop asamp

save "lasi03pub.dta", replace

describe _all, fullnames
summarize _all
count

summarize _all if (r1agey < 110)
count if (r1agey < 110)

// -------------------------------------------------------------------
set linesize 80
log close
exit

// EOF
