clear 
clear matrix 
set mem 500m 
set more off 
clear mata
set matsize 11000
set maxvar 11000 
pause off


/*********************************************************************/
/*********************************************************************/ 
 
macro define ACS       "" 
macro define ENFOR     "" 
macro define RESULTS   "" 
macro define CODE      "" 



/*********************************************************************/
/*********************************************************************/ 
 
/*** Set Up Main Data Set for Analysis ****/

use $ACS/data_mechanisms.dta, clear   

cap drop _merge

* Set Up Controls
global controls "age age2 black married nchlt5 nchild college master phd"
global controls_noedu "age age2 black married child5 children"
global controls_nokids "age age2 black married  college master phd"
global controls_noedu_nokids "age age2 black married  "
global controls_eventstudy "age age2 black married nchlt5 nchild college master phd"
global controls_econ "shift_share_sample shift_share_sample_immig shift_share_sample_hskillw shift_share_sample_hskillm "
global controls_econ2 "shift_share_sample shift_share_sample_immig  "
global controls_287g "jail287g task287g"
global trendsdiff "cit_diff black_diff noncit_diff lfp_diff child5_diff children_diff uhrs50_c_diff uhrs60_c_diff  college_diff master_diff phd_diff wcollege_diff wmaster_diff wphd_diff urate_diff HPIwith2000base_diff"


**********************************************************************************************
* TABLE 6 
**********************************************************************************************

set more off
pause off

eststo clear
foreach sector in hhs  {  //  housekeep childcare
foreach aweight in  cpuma0010_pop  {
**************************************************
*1. Collapse Data to `p' level
************************************************** 
preserve

replace lfp =lfp/100


local grouplist "   ls_hsp_fb ls_hsp_mc  ls_hsp_mc80 ls ls_nfb ls_nohsp_nfb ls_hsp_nfb"
foreach group in `grouplist'    {
foreach demog in   `sector'_`group'_w    {
gen lfp_`demog' = lfp if `demog'==1 // only count observations if they are BOTH LEFB and working in right type of jobs
gen uhr_`demog' = uhrswork if `demog'==1 // only count observations if they are BOTH LEFB and working in right type of jobs
  }
  }
  

  collapse  (rawsum) weight (sum)  lfp_* uhr_*  all uhrswork  lfp (max)   ///
  statefip puma [fw=perwt] , by(cpuma0010 year) 
  
**************************************************
*2. Merge in Control and Enforcement Variables at 	cpuma0010 level 
************************************************** 
cap drop _merge
do "$CODE/acs_mergecontrolscode_feb2022.do"

merge m:1 cpuma0010  using "$ACS/temp_pop.dta" 
tab year _merge
sum statefip puma if _merge==1
sum statefip puma if _merge==2 & year<=2011  
keep if _merge==3 
drop _merge 

merge m:1 cpuma0010 year using "$ACS/temp_pop2.dta" 
tab year _merge
sum statefip puma if _merge==1
sum statefip puma if _merge==2 & year<=2011  
keep if _merge==3 
drop _merge 

merge m:1 cpuma0010 using "$ACS/pop_nc.dta"
drop _merge

merge m:1 cpuma0010 using "$ENFOR/final_deportations_cpuma0010.dta"
drop _merge


**************************************************
*3. Set Up Regressions * MAIN TABLE *
************************************************** 

local grouplist2 "ls_hsp_fb ls_hsp_mc  ls_hsp_mc80 "


  cap drop u2*
// time varying total hours worked in group&sector / time varying total pop 
foreach group in `grouplist2'    {
foreach demog in   `sector'_`group'_w    {
gen u2_`demog' = (lfp_`demog'/pop_all)*100  

qui eststo: reghdfe u2_`demog' $controls_econ   $controls_287g $trendsdiff SC   [aw=`aweight'], absorb(cpuma0010 year) vce (cluster cpuma0010)  
estadd ysumm	
	test SC =  0  
	estadd scalar p = r(p)
	esttab, keep(SC*) se
	sum u2_`demog'  [aweight=`aweight']
	estadd scalar perc = (_b[SC]/r(mean))*100
	  }
  }
 

esttab   using "$RESULTS/table6.tex", keep(SC ) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes replace ///
 mtitles("LE HISP FB"  "LE HISP CA/MX"  "LE HISP CA/MX 80+"  )  ///
stats(  ymean p perc  N, ///
 labels ( "Mean Y"  "P-Value SC" "\% Effect" "Observations") fmt( 2 2 2 0) ) 	nonumbers prefoot("") postfoot("") label ///
 	varlabels( , blist(SC "\midrule \it{\underline{A: (Total \# Work in Household Services / Total PUMA Pop) *100}} \\ "))
eststo clear

  
 cap drop u2*
// time varying total hours worked in group&sector / time varying total pop 
foreach group in `grouplist2'    {
foreach demog in   `sector'_`group'_w    {
gen u2_`demog' = (uhr_`demog'/pop_all)*100  

qui eststo: reghdfe u2_`demog' $controls_econ   $controls_287g $trendsdiff SC   [aw=`aweight'], absorb(cpuma0010 year) vce (cluster cpuma0010)  
estadd ysumm	
	test SC =  0  
	estadd scalar p = r(p)
	esttab, keep(SC*) se
	sum u2_`demog'  [aweight=`aweight']
	estadd scalar perc = (_b[SC]/r(mean))*100
	  }
  }
 
	
esttab   using "$RESULTS/table6.tex", keep(SC ) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes append ///
 mtitles("" "" "" "" "" "" "" "" "" "" "" "" "" "" "")  ///
stats(  ymean p  perc N, ///
 labels ( "Mean Y"   "P-Value SC" "\% Effect" "Observations") fmt( 2 2 2 0) ) 	nonumbers  prefoot("") prehead("") posthead("")	  label ///
 	varlabels( , blist(SC "\midrule \it{\underline{B: (Total \# Hours Work in Household Services / Total PUMA Pop) *100}} \\ "))
eststo clear



 

**************************************************
*TABLE A5  *
************************************************** 

local grouplist3 "ls ls_nfb ls_nohsp_nfb ls_hsp_nfb"

 cap drop u2*
// time varying total hours worked in group&sector / time varying total pop 
foreach group in `grouplist3'    {
foreach demog in   `sector'_`group'_w    {
gen u2_`demog' = (uhr_`demog'/pop_all)*100  

qui eststo: reghdfe u2_`demog' $controls_econ   $controls_287g $trendsdiff SC   [aw=`aweight'], absorb(cpuma0010 year) vce (cluster cpuma0010)  
estadd ysumm	
	test SC =  0  
	estadd scalar p = r(p)
	esttab, keep(SC*) se
	sum u2_`demog'  [aweight=`aweight']
	estadd scalar perc = (_b[SC]/r(mean))*100
	  }
  }
 
esttab   using "$RESULTS/tablea5.tex", keep(SC ) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes replace ///
 mtitles("All Low-Edu" "Low-Edu USB" "Low-edu USB Non-His" "Low-Edu USB Hispanic")  ///
stats(  ymean p perc N, ///
 labels ( "Mean Y"  "P-Value SC" "\% Effect" "Observations") fmt( 2 2 2 0) ) 	nonumbers 	 label 
eststo clear




restore
}
}



**********************************************************************************************
* TABLE 7
**********************************************************************************************

use $ACS/data_mechanisms.dta, clear   


cap drop _merge

* Set Up Controls
global controls "age age2 black married nchlt5 nchild college master phd"
global controls_noedu "age age2 black married child5 children"
global controls_nokids "age age2 black married  college master phd"
global controls_noedu_nokids "age age2 black married  "
global controls_eventstudy "age age2 black married nchlt5 nchild college master phd"
global controls_econ "shift_share_sample shift_share_sample_immig shift_share_sample_hskillw shift_share_sample_hskillm "
global controls_econ2 "shift_share_sample shift_share_sample_immig  "
global controls_287g "jail287g task287g"
global trendsdiff "cit_diff black_diff noncit_diff lfp_diff child5_diff children_diff uhrs50_c_diff uhrs60_c_diff  college_diff master_diff phd_diff wcollege_diff wmaster_diff wphd_diff urate_diff HPIwith2000base_diff"


**********************************************************************************************
* NEW TABLE INTENSITY 
**********************************************************************************************

set more off
pause off

foreach sector in hhs  {  //  housekeep childcare
foreach aweight in cpuma0010_pop  {
**************************************************
*1. Collapse Data to `p' level
************************************************** 

preserve
replace lfp =lfp/100


local grouplist2 "ls_hsp_fb ls_hsp_mc ls_hsp_mc80 "


foreach group in `grouplist2'    {
foreach demog in   `sector'_`group'_w    {
gen lfp_`demog' = lfp if `demog'==1 // only count observations if they are BOTH LSNC and working in right type of jobs
gen uhr_`demog' = uhrswork if `demog'==1 // only count observations if they are BOTH LSNC and working in right type of jobs
  }
  }
  

  collapse  (rawsum) weight (sum)  lfp_* uhr_*  all uhrswork  lfp (max)   ///
  statefip puma [fw=perwt] , by(cpuma0010 year) 
  
**************************************************
*2. Merge in Control and Enforcement Variables at 	cpuma0010 level 
************************************************** 
cap drop _merge
do "$CODE/acs_mergecontrolscode_feb2022.do"

merge m:1 cpuma0010  using "$ACS/temp_pop.dta" 
tab year _merge
sum statefip puma if _merge==1
sum statefip puma if _merge==2 & year<=2011  
keep if _merge==3 
drop _merge 


merge m:1 cpuma0010 year using "$ACS/temp_pop2.dta" 
tab year _merge
sum statefip puma if _merge==1
sum statefip puma if _merge==2 & year<=2011  
keep if _merge==3 
drop _merge 


merge m:1 cpuma0010 using "$ACS/pop_nc_alt.dta"
drop _merge


merge m:1 cpuma0010 using "$ENFOR/final_deportations_cpuma0010.dta"
drop _merge

merge m:1 cpuma0010 year using "$ENFOR/ice_policies_cpuma.dta"
drop if _merge~=3
drop _merge


**************************************************
*3. Set Up Regressions * MAIN TABLE *
************************************************** 

local grouplist2 "ls_hsp_fb ls_hsp_mc ls_hsp_mc80 "

for any  mscc_notserious hispanic_nosp centralmx mscc_immigration mscc_dui latin mexico : rename frac_X share_X



* cap drop u2*
// time varying total hours worked in group&sector / time varying total pop 
foreach group in `grouplist2'    {
foreach demog in   `sector'_`group'_w    {
gen u2_`demog' = (uhr_`demog'/pop_all)*100  

eststo: reghdfe u2_`demog' $controls_econ   $controls_287g $trendsdiff   SC   [aw=`aweight'], absorb(cpuma0010 year) vce (cluster cpuma0010)  
estadd ysumm	
	test SC =  0  
	estadd scalar p = r(p)
	esttab, keep(SC*) se

	  }
  }
 
 esttab   using "$RESULTS/table7.tex", keep(SC ) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes replace ///
mtitles("LE HISP FB" "LE HISP CAMX" "LE HISP CAMX 80+") ///
stats(  ymean p  N, ///
 labels ( "Mean Y"  "P-Value SC" "N") fmt( 2 2 0) ) 	nonumbers prefoot("") postfoot("") label ///
 	varlabels( , blist(SC "\midrule \it{\underline{A: (Total \# Hours Work in Household Services / Total PUMA Pop) *100}} \\ "))
eststo clear


foreach int in ls_hsp_fb hispanic_nosp centralmx  mexico  {
sum share_`int'
gen `int'_SC=share_`int'*SC
label var `int'_SC "SC*`int'"

foreach group in `grouplist2'    {
foreach demog in   `sector'_`group'_w    {

eststo: reghdfe u2_`demog' $controls_econ   $controls_287g $trendsdiff   SC `int'_SC   [aw=`aweight'], absorb(cpuma0010 year) vce (cluster cpuma0010)  
estadd ysumm
	sum share_`int' [aw=`aweight'] if e(sample)==1
	estadd scalar frac = r(mean)
	estadd scalar sd = r(sd)
	estadd scalar b_atmean = _b[SC]+(_b[`int'_SC]*r(mean))
	estadd scalar b_atmeansd = _b[SC]+(_b[`int'_SC]*(r(mean)+r(sd)))	
	test SC =  0  
	estadd scalar p = r(p)
	test SC `int'_SC
	estadd scalar p_int = r(p)
	esttab, keep(*SC*) se se(3) b(3) star(* 0.10 ** 0.05 *** 0.01)
	}
	}
	  
 esttab   using "$RESULTS/table7.tex", keep(SC `int'_SC) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes append ///
 mtitles( "" "" "" "" "" "" "" "" "" "" "" "" "")  ///
 stats(  ymean frac sd b_atmean b_atmeansd p p_int  N, ///
		labels ( "Mean Y" "Mean Intensity" "SD Intensity" "$\beta$--Mean Int" "$\beta$--1 SD Higher Int" "P-Value SC" "P-Value SC \& Interaction" "N") fmt( 2 2 2 2 2 2 2 0) ) ///
		nonumbers  prefoot("") postfoot("") prehead("") posthead("")  label ///
 	varlabels( , blist(SC "\midrule \it{\underline{B: (Total \# Hours Work in Household Services / Total PUMA Pop) *100}} \\ "))
eststo clear
}


foreach int in mscc_notserious    {
gen `int'_SC=share_`int'*SC
label var `int'_SC "SC*`int'"

cap drop intn_*
foreach var in $controls_econ $controls_287g {
gen intn_`var' = `var'*share_`int'
}


foreach group in `grouplist2'    {
foreach demog in   `sector'_`group'_w    {

eststo: reghdfe u2_`demog' $controls_econ   $controls_287g $trendsdiff   SC `int'_SC   [aw=`aweight'], absorb(cpuma0010 year) vce (cluster cpuma0010)  
estadd ysumm
	sum share_`int' [aw=`aweight'] if e(sample)==1
	estadd scalar frac = r(mean)
	estadd scalar sd = r(sd)
	estadd scalar b_atmean = _b[SC]+(_b[`int'_SC]*r(mean))
	estadd scalar b_atmeansd = _b[SC]+(_b[`int'_SC]*(r(mean)+r(sd)))	
	test SC =  0  
	estadd scalar p = r(p)
	test SC `int'_SC
	estadd scalar p_int = r(p)
	esttab, keep(*SC*) se se(3) b(3) star(* 0.10 ** 0.05 *** 0.01)
	}
	}

 esttab   using "$RESULTS/table7.tex", keep(SC `int'_SC) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes append ///
 mtitles( "" "" "" "" "" "" "" "" "" "" "" "" "")  ///
 stats(  ymean frac sd b_atmean b_atmeansd p p_int  N, ///
		labels ( "Mean Y" "Mean Intensity" "SD Intensity" "$\beta$--Mean Int" "$\beta$--1 SD Higher Int" "P-Value SC" "P-Value SC \& Interaction" "N") fmt( 2 2 2 2 2 2 2 0) ) ///
		nonumbers  prefoot("") prehead("") posthead("")   label ///
 	varlabels( , blist(SC "\midrule \it{\underline{X: (Total \# Hours Work in Household Services / Total PUMA Pop) *100}} \\ "))
eststo clear
}

  restore
  }
  
  }
  

**********************************************************************************************
* TABLE 8 AND FIGURE 4  
**********************************************************************************************

use $ACS/data_mechanisms.dta, clear  

replace incwage=. if incwage>999997
replace incwage =incwage/annual_cpi

gen wkswork3 = 5.5 if wkswork2==1
replace wkswork3 = 21 if wkswork2==2
replace wkswork3 = 32 if wkswork2==3
replace wkswork3 = 43 if wkswork2==4
replace wkswork3 = 48.5 if wkswork2==5
replace wkswork3 = 51 if wkswork2==6

gen logwage2=log(incwage/(wkswork3*uhrswork))

cap drop _merge

drop if qincwage==4

* Set Up Controls
global controls "age age2 black married nchlt5 nchild college master phd"
global controls_noedu "age age2 black married child5 children"
global controls_nokids "age age2 black married  college master phd"
global controls_noedu_nokids "age age2 black married  "
global controls_eventstudy "age age2 black married nchlt5 nchild college master phd"
global controls_econ "shift_share_sample shift_share_sample_immig shift_share_sample_hskillw shift_share_sample_hskillm "
global controls_287g "jail287g task287g"
global trendsdiff "cit_diff black_diff noncit_diff lfp_diff child5_diff children_diff uhrs50_c_diff uhrs60_c_diff  college_diff master_diff phd_diff wcollege_diff wmaster_diff wphd_diff urate_diff HPIwith2000base_diff"

do "$CODE/acs_mergecontrolscode_feb2022.do"


local yvar "logwage2"  


eststo clear
foreach group in hhs_all_w   hhs_ls_w  {  
foreach x of local yvar{ 
eststo: reghdfe `x'    SC  $controls $controls_econ   $controls_287g $trendsdiff  if `group'==1  [aw=weight], absorb(cpuma0010 year) vce (cluster cpuma0010)  
	estadd ysumm
	test SC =  0  
	estadd scalar p = r(p)

}
}

esttab   using "$RESULTS/table8.tex", keep(SC ) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes replace ///
 mtitles("All Females" "Low-Edu Females" )  ///
stats(  ymean p  N, ///
 labels ( "Mean Y"  "P-Value SC" "N") fmt( 2 2 0) ) 	nonumbers label ///
 	varlabels( , blist(SC "\midrule \it{\underline{Log(Hourly Wages)}} \\ "))
eststo clear



**********************************************************************************************
* EVENT STUDIES *
**********************************************************************************************

merge m:1 cpuma0010  using "$ACS/temp_pop.dta" 

tab year _merge
sum statefip puma if _merge==1
sum statefip puma if _merge==2 & year<=2011  
keep if _merge==3 
drop _merge 

merge m:1  cpuma0010 year using "$ACS/eventstudy.dta"
tab year _merge
sum statefip puma if _merge==1
sum statefip puma if _merge==2 & year<=2011 //not merging are pumas that changed codes because of katrina, HI, and VA
keep if _merge==3 
drop _merge 



foreach group in  hhs_all_w   hhs_ls_w      { //hhs_all hhs_ls
preserve
gen w = 1
keep if `group'==1
collapse (rawsum) w (mean)  SC  [aw=weight] , by(cpuma0010 year )
rename w weight

sum weight
bysort cpuma0010: gen n = _N
sum n
pause
drop if n~=10
keep cpuma0010
gen balance=1
duplicates drop
sum
tempfile cpuma_balance_`group'
save "`cpuma_balance_`group''", replace
restore
}


pause off
foreach group in  hhs_all_w   hhs_ls_w      { //hhs_all hhs_ls

preserve

* Sample Restrictions
keep if `group'==1


merge m:1 cpuma0010 using "`cpuma_balance_`group''"
keep if balance==1

**************************************************
*3. Set Up Regressions
**************************************************
gen flag = (event_time==0 & year==2008)
bysort cpuma0010: egen max_flag=max(flag)

gen event_time_temp = event_time
replace event_time = event_time_temp
tab event_time year

cap drop et_I_*
qui tab event_time , gen(et_I_)
sum et_I_*
sum et_I_* if event_time==0
pause
replace et_I_7=0
replace et_I_9=0

forvalues e = 1/15 {
local e2 = `e'-9
label var et_I_`e' "`e2'"
}




local yvar "   logwage2 "

foreach x of local yvar{


			
			 
			 reghdfe `x' et_I_* [aw=weight], absorb(cpuma0010 year) vce (cluster cpuma0010) 
					
			coefplot, keep(et_I_4 et_I_5 et_I_6 et_I_7 et_I_8 et_I_9 et_I_10 et_I_11 et_I_12   )  vertical omitted  xline(6.5) yline(0) xtitle("Event Time (years)") ytitle("Beta")

			graph export "$RESULTS/figure4_`group'.png", replace  height(600) width(800)
				
			

}


 restore
}



**********************************************************************************************
* FIGURE 3  
**********************************************************************************************

use $ACS/data_mechanisms.dta, clear   

cap drop _merge

* Set Up Controls
global controls "age age2 black married nchlt5 nchild college master phd"
global controls_noedu "age age2 black married child5 children"
global controls_nokids "age age2 black married  college master phd"
global controls_noedu_nokids "age age2 black married  "
global controls_eventstudy "age age2 black married nchlt5 nchild college master phd"
global controls_econ "shift_share_sample shift_share_sample_immig shift_share_sample_hskillw shift_share_sample_hskillm "
global controls_econ2 "shift_share_sample shift_share_sample_immig  "
global controls_287g "jail287g task287g"
global trendsdiff "cit_diff black_diff noncit_diff lfp_diff child5_diff children_diff uhrs50_c_diff uhrs60_c_diff  college_diff master_diff phd_diff wcollege_diff wmaster_diff wphd_diff urate_diff HPIwith2000base_diff"


set more off
pause off

eststo clear
foreach sector in hhs  {  //  housekeep childcare
foreach aweight in cpuma0010_pop  {
**************************************************
*1. Collapse Data to `p' level
************************************************** 
preserve

replace lfp =lfp/100

local grouplist "ls_hsp_fb "

foreach group in `grouplist'    {
foreach demog in   `sector'_`group'_w    {
cap gen lfp_`demog' = lfp if `demog'==1 // only count observations if they are BOTH LSNC and working in right type of jobs
cap gen uhr_`demog' = uhrswork if `demog'==1 // only count observations if they are BOTH LSNC and working in right type of jobs
  }
  }
  

  collapse  (rawsum) weight (sum)  lfp_* uhr_*  all uhrswork  lfp (max)   ///
  statefip puma [fw=perwt] , by(cpuma0010 year) 
  

  
**************************************************
*2. Merge in Control and Enforcement Variables at 	cpuma0010 level 
************************************************** 
cap drop _merge
do "$CODE/acs_mergecontrolscode_feb2022.do"

merge m:1 cpuma0010  using "$ACS/temp_pop.dta" 
tab year _merge
sum statefip puma if _merge==1
sum statefip puma if _merge==2 & year<=2011  
keep if _merge==3 
drop _merge 

merge m:1 cpuma0010 year using "$ACS/temp_pop2.dta" 
tab year _merge
sum statefip puma if _merge==1
sum statefip puma if _merge==2 & year<=2011  
keep if _merge==3 
drop _merge 


merge m:1 cpuma0010 year using "$ACS/eventstudy.dta"
tab year _merge
sum statefip puma if _merge==1
sum statefip puma if _merge==2 & year<=2011 //not merging are pumas that changed codes because of katrina, HI, and VA
keep if _merge==3 
drop _merge 


gen event_time_temp = event_time
replace event_time = event_time_temp


**************************************************
*3. Set Up Regressions
************************************************** 


cap drop et_I_*
qui tab event_time , gen(et_I_)
sum et_I_*
sum et_I_* if event_time==0
pause
replace et_I_7=0
replace et_I_9=0

forvalues e = 1/15 {
local e2 = `e'-9
label var et_I_`e' "`e2'"
}


  
 cap drop u2*
// time varying total hours worked in group&sector / time varying total pop 
foreach group in `grouplist'    {
foreach demog in   `sector'_`group'_w    {
gen u2_`demog' = (uhr_`demog'/pop_all)*100  


coefplot, keep(et_I_4 et_I_5 et_I_6 et_I_7 et_I_8 et_I_9 et_I_10 et_I_11 et_I_12   )  vertical omitted xline(6.5) ylabel(-4(1)4)  yline(0) xtitle("Event Time (years)") ytitle("Beta")
graph export "$RESULTS/figure3.png", replace  height(600) width(800)

	  }
  }
 

  restore
  }
  }
 

