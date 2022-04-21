clear 
clear matrix 
set mem 500m 
set more off 
clear mata
set matsize 11000
set maxvar 11000 
pause off

macro define ACS       "" 
macro define ENFOR     "" 
macro define RESULTS   "" 
macro define CODE      "" 



/*********************************************************************/
/* MAIN ANALYSES */  
/*********************************************************************/


* Set Up Controls
global controls "age age2 black married nchlt5 nchild college master phd"
global controls_nokids "age age2 black married  college master phd"
global controls_eventstudy "age age2 black married nchlt5 nchild college master phd"
global controls_econ "shift_share_sample shift_share_sample_immig shift_share_sample_hskillw shift_share_sample_hskillm "
global controls_287g "jail287g task287g"
global trendsdiff "cit_diff black_diff noncit_diff lfp_diff child5_diff children_diff uhrs50_c_diff uhrs60_c_diff  college_diff master_diff phd_diff wcollege_diff wmaster_diff wphd_diff urate_diff HPIwith2000base_diff"

use $ACS/analysis_data.dta, clear


set more off


****************************************************************************************************
*   SUMMARY STATISTICS -- Table 1 *
****************************************************************************************************
pause off
foreach group in hsusbwom  hsusbwom_kids hsusbwom_ykids hsusbwom_nokids hsusbmen  { // {

preserve

**************************************************
*1. Sample Restrictions 
************************************************** 

 
keep if `group'==1  

**************************************************
*2. Merge in Control and Enforcement Variables at 	cpuma0010 level 
************************************************** 
qui do "$CODE/acs_mergecontrolscode_feb2022.do"

cap drop sample
gen sample =1
foreach var in lfp uhrswork  $controls_econ $controls_287g $controls SC $trendsdiff {
replace sample = 0 if `var'==.
}
keep if sample==1

summ $controls lfp uhrswork SC [aw=weight]

// mean hours worked among workers in base years
summ uhrswork if year==2005 & lfp==100 [aw=weight]


restore
}



	
	

****************************************************************************************************
* Table 11 *
****************************************************************************************************
preserve
drop if year>2014

**************************************************
*1. Sample Restrictions 
************************************************** 
keep if hsusbmen==1

**************************************************
*2. Merge in Control and Enforcement Variables at 	cpuma0010 level 
************************************************** 
do "$CODE/acs_mergecontrolscode_feb2022.do"

**************************************************
*3. Set Up Regressions
**************************************************
local yvar "uhrswork"  


eststo clear
foreach group in hsusbmen hsusbmen_kids hsusbmen_ykids  hsusbmen_nokids {  
foreach x of local yvar{ 
qui eststo: reghdfe `x' $controls_econ $controls_287g $trendsdiff $controls  SC if `group'==1  [aw=weight], absorb(cpuma0010 year) vce (cluster cpuma0010)  
	estadd ysumm
	test SC =  0  
	estadd scalar p = r(p)
	sum `x' if e(sample)==1  [aw=weight]
	estadd scalar perc = (_b[SC]/r(mean))*100

}
}

esttab, keep(SC) se

	esttab   using "$RESULTS/addcontrols_indiv_usbmen.tex", keep(SC) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes replace ///
	mgroups( "All" "Kids in HHold"   "No Kids in Hhold" , ///
	pattern(1 1 0 0 0 1  ) 	prefix(\multicolumn{@span}{c}{) suffix(}) span  erepeat(\cmidrule(lr){@span})) mtitles("" "Kids of Any Age" "Kids Under 5"  "None of Any Age")  ///
	stats(  ymean p perc N, ///
	labels ( "Mean Y" "P-Value" "\% Effect" "N") fmt( 2 2 2 0) ) ///
	nonumbers   label 
	
	eststo clear
	restore


****************************************************************************************************
* Table 10 *
****************************************************************************************************
preserve
drop if year>2014

**************************************************
*1. Sample Restrictions 
************************************************** 
keep if hsusbmen==1 | hsusbwom==1

**************************************************
*2. Merge in Control and Enforcement Variables at 	cpuma0010 level 
************************************************** 
do "$CODE/acs_mergecontrolscode_feb2022.do"

**************************************************
*3. Set Up Regressions
**************************************************
local yvar "uhrswork"  

foreach x of local yvar{ 
eststo clear
foreach group in  ykids   {  
cap drop SC_treat
gen SC_treat = SC if hsusbwom_kids==1
replace SC_treat=0 if hsusbwom_nokids==1
replace hsusbwom_`group'=0 if hsusbwom_`group'==.
label var SC_treat "SC * Mothers with Young Kids"
 qui eststo: reghdfe `x' $controls_econ $controls_287g $trendsdiff $controls hsusbwom_`group' SC SC_treat if hsusbwom_nokids==1 | hsusbwom_`group'==1  [aw=weight], ///
 absorb(cpuma0010 year cpuma0010#hsusbwom_`group' year#hsusbwom_`group') vce (cluster cpuma0010)  
	estadd ysumm
	test SC_treat =  0  
	estadd scalar p = r(p)
	sum `x' if hsusbwom_`group'==1 & e(sample)==1
	estadd scalar NT = r(N)
	sum `x' if hsusbwom_nokids==1 & e(sample)==1 
	estadd scalar NC = r(N)
	
qui eststo: reghdfe `x' $controls_econ $controls_287g $trendsdiff $controls hsusbwom_`group'  SC_treat if hsusbwom_nokids==1 | hsusbwom_`group'==1  [aw=weight], ///
 absorb(cpuma0010 year cpuma0010#year cpuma0010#hsusbwom_`group' year#hsusbwom_`group') vce (cluster cpuma0010)  
	estadd ysumm
	test SC_treat =  0  
	estadd scalar p = r(p)
	sum `x' if hsusbwom_`group'==1  & e(sample)==1
	estadd scalar NT = r(N)
	sum `x' if hsusbwom_nokids==1  & e(sample)==1
	estadd scalar NC = r(N)
}


esttab, keep(SC SC_treat) se star(* 0.10 ** 0.05 *** 0.01) stats(  ymean p  N, ///
	labels ( "Mean Y" "P-Value" "N") fmt( 2 2 0) ) 

esttab   using "$RESULTS/triplediff.tex", keep(SC SC_treat) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes replace ///
mgroups( "Without PUMA*Year FE" "With PUMA*Year FE" , ///
 pattern(1 1   ) prefix(\multicolumn{@span}{c}{) suffix(}) span  erepeat(\cmidrule(lr){@span})) ///
 mtitles("" "")  ///
stats(  ymean p  NT NC, ///
 labels ( "Mean Y" "P-Value on SC * Mothers with Young Kids"  "N - Mothers of Young Kids"  "N - Comparison Group") fmt( 2 2 0 0) ) nonumbers prefoot("") postfoot("")  label ///
 	varlabels( , blist(SC "\midrule \it{\underline{A: Comparison Group is Females without Kids}} \\ "))
}

eststo clear

local yvar "uhrswork"  

foreach x of local yvar{ 
eststo clear
foreach group in  ykids  {  
cap drop SC_treat
gen SC_treat = SC*hsusbwom_`group'
replace SC_treat=0 if hsusbmen_`group'==1
sum SC SC_treat if hsusbwom_`group'==1
sum SC SC_treat if hsusbmen_`group'==1
replace hsusbwom_`group'=0 if hsusbwom_`group'==.
label var SC_treat "SC * Mothers with Young Kids"

*cap drop tdc_*
*for any $controls: gen tdc_X = X*hsusbwom_`group'

qui eststo: reghdfe `x' $controls_econ $controls_287g $trendsdiff $controls   hsusbwom_`group' SC SC_treat if hsusbmen_`group'==1 | hsusbwom_`group'==1  [aw=weight], ///
 absorb(cpuma0010 year cpuma0010#hsusbwom_`group' year#hsusbwom_`group') vce (cluster cpuma0010)  
	estadd ysumm
	test SC_treat =  0  
	estadd scalar p = r(p)
	sum `x' if hsusbwom_`group'==1  & e(sample)==1
	estadd scalar NT = r(N)
	sum `x' if hsusbmen_`group'==1  & e(sample)==1
	estadd scalar NC = r(N)
	
qui eststo: reghdfe `x' $controls_econ $controls_287g $trendsdiff $controls   hsusbwom_`group' SC SC_treat if hsusbmen_`group'==1 | hsusbwom_`group'==1  [aw=weight], ///
 absorb(cpuma0010 year cpuma0010#year cpuma0010#hsusbwom_`group' year#hsusbwom_`group') vce (cluster cpuma0010)	
qui eststo: reghdfe `x' $controls_econ $controls_287g $trendsdiff $controls   hsusbwom_`group'  SC_treat if hsusbmen_`group'==1 | hsusbwom_`group'==1  [aw=weight], ///
 absorb(cpuma0010 year cpuma0010#year cpuma0010#hsusbwom_`group' year#hsusbwom_`group') vce (cluster cpuma0010)  
	estadd ysumm
	test SC_treat =  0  
	estadd scalar p = r(p)
	sum `x' if hsusbwom_`group'==1  & e(sample)==1
	estadd scalar NT = r(N)
	sum `x' if hsusbmen_`group'==1  & e(sample)==1
	estadd scalar NC = r(N)

esttab, keep(SC SC_treat) se star(* 0.10 ** 0.05 *** 0.01) 
}

esttab, keep(SC SC_treat) se star(* 0.10 ** 0.05 *** 0.01) stats(  ymean p  N, ///
	labels ( "Mean Y" "P-Value" "N") fmt( 2 2 0) ) 

	esttab   using "$RESULTS/triplediff.tex", keep(SC SC_treat) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes append ///
	mtitles("" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "")  stats(  ymean p  NT NC, ///
 labels ( "Mean Y" "P-Value on SC * Mothers with Young Kids"  "N - Mothers of Young Kids"  "N - Comparison Group") fmt( 2 2 0 0) )  ///
	nonumbers prefoot("") prehead("") posthead("")  label ///
 	varlabels( , blist(SC "\midrule \it{\underline{B: Comparison Group is Fathers with Young Kids}} \\ "))
	
}
eststo clear
restore


	
	
	
	
	
	
	
	
	
	
	
	
keep if hsusbwom==1

****************************************************************************************************
* Table 3 *
****************************************************************************************************
preserve
drop if year>2014

**************************************************
*1. Sample Restrictions 
************************************************** 
keep if hsusbwom==1

**************************************************
*2. Merge in Control and Enforcement Variables at 	cpuma0010 level 
************************************************** 
do "$CODE/acs_mergecontrolscode_feb2022.do"

**************************************************
*3. Set Up Regressions
**************************************************
local yvar "uhrswork"  

eststo clear
foreach group in  hsusbwom hsusbwom_kids hsusbwom_ykids hsusbwom_nokids {  
foreach x of local yvar{ 
qui eststo: reghdfe `x'    SC  if `group'==1  [aw=weight], absorb(cpuma0010 year) vce (cluster cpuma0010)  
	estadd ysumm
	test SC =  0  
	estadd scalar p = r(p)
	sum `x' if e(sample)==1  [aw=weight]
	estadd scalar perc = (_b[SC]/r(mean))*100

}
}

esttab, keep(SC) se

esttab   using "$RESULTS/addcontrols_indiv_usbwom.tex", keep(SC) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes replace ///
mgroups( "All""Kids in HHold" "No Kids in Hhold" , ///
 pattern(1 1 0 1  ) prefix(\multicolumn{@span}{c}{) suffix(}) span  erepeat(\cmidrule(lr){@span})) mtitles("" "Kids of Any Age" "Kids Under 5" "Without Kids")  ///
stats(  ymean p perc N, ///
 labels ( "Mean Y" "P-Value" "\% Effect" "N") fmt( 2 2 2 0) ) nonumbers prefoot("") postfoot("")  label ///
 	varlabels( , blist(SC "\midrule \it{\underline{A: PUMA FE, Year FE}} \\ "))

	eststo clear
foreach group in  hsusbwom hsusbwom_kids hsusbwom_ykids hsusbwom_nokids {  
foreach x of local yvar{ 
qui eststo: reghdfe `x' $controls_econ $controls_287g   SC  if `group'==1   [aw=weight], absorb(cpuma0010 year) vce (cluster cpuma0010)  
	estadd ysumm
	test SC =  0  
	estadd scalar p = r(p)
	sum `x' if e(sample)==1  [aw=weight]
	estadd scalar perc = (_b[SC]/r(mean))*100
}
}

esttab, keep(SC) se


	esttab   using "$RESULTS/addcontrols_indiv_usbwom.tex", keep(SC) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes append ///
	mtitles("" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "")  stats(  ymean p  N, ///
	labels ( "Mean Y" "P-Value" "N") fmt( 2 2 0) ) ///
	nonumbers prefoot("") postfoot("")	prehead("") posthead("")   label ///
 	varlabels( , blist(SC "\midrule \it{\underline{B: Add PUMA-Year Controls}} \\ "))
	
	
	eststo clear
foreach group in  hsusbwom hsusbwom_kids hsusbwom_ykids hsusbwom_nokids {  
foreach x of local yvar{ 
qui eststo: reghdfe `x' $controls_econ $controls_287g $trendsdiff   SC  if `group'==1   [aw=weight], absorb(cpuma0010 year) vce (cluster cpuma0010)  
	estadd ysumm
	test SC =  0  
	estadd scalar p = r(p)
	sum `x' if e(sample)==1  [aw=weight]
	estadd scalar perc = (_b[SC]/r(mean))*100
}
}

esttab, keep(SC) se


	esttab   using "$RESULTS/addcontrols_indiv_usbwom.tex", keep(SC) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes append ///
	mtitles("" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "")  stats(  ymean p perc N, ///
 labels ( "Mean Y" "P-Value" "\% Effect" "N") fmt( 2 2 2 0) )  ///
	nonumbers prefoot("") postfoot("")	prehead("") posthead("")   label ///
 	varlabels( , blist(SC "\midrule \it{\underline{C: Add PUMA Characteristic Trends}} \\ "))


eststo clear
foreach group in  hsusbwom hsusbwom_kids hsusbwom_ykids hsusbwom_nokids {  
foreach x of local yvar{ 
qui eststo: reghdfe `x' $controls_econ $controls_287g $trendsdiff $controls  SC   if `group'==1  [aw=weight], absorb(cpuma0010 year) vce (cluster cpuma0010)  
	estadd ysumm
	test SC =  0  
	estadd scalar p = r(p)
	sum `x' if e(sample)==1  [aw=weight]
	estadd scalar perc = (_b[SC]/r(mean))*100
}
}

esttab, keep(SC) se

	esttab   using "$RESULTS/addcontrols_indiv_usbwom.tex", keep(SC) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes append ///
	mtitles("" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "")  stats(  ymean p perc N, ///
 labels ( "Mean Y" "P-Value" "\% Effect" "N") fmt( 2 2 2 0) )  ///
	nonumbers prefoot("") prehead("") posthead("")  label ///
 	varlabels( , blist(SC "\midrule \it{\underline{D: Add Demographics}} \\ "))
	
	
	eststo clear
	
	restore





	
	



****************************************************************************************************
* Table 4 *
****************************************************************************************************
foreach group in  hsusbwom_kids hsusbwom_ykids hsusbwom_nokids {  

preserve
drop if year>2014

**************************************************
*1. Sample Restrictions 
************************************************** 
keep if `group'==1

**************************************************
*2. Merge in Control and Enforcement Variables at 	cpuma0010 level 
************************************************** 
do "$CODE/acs_mergecontrolscode_feb2022.do"

**************************************************
*3. Set Up Regressions
**************************************************

 
local yvar "lfp uhrs_c uhrs35plus uhrs2035 uhrs20less "  

eststo clear  
foreach x of local yvar{ 
qui eststo: reghdfe `x' $controls_econ $controls_287g $trendsdiff $controls  SC   [aw=weight], absorb(cpuma0010 year) vce (cluster cpuma0010)  
	estadd ysumm
	test SC =  0  
	estadd scalar p = r(p)
	sum `x' if e(sample)==1  [aw=weight]
	estadd scalar perc = (_b[SC]/r(mean))*100
}

esttab, keep(SC) se

 if "`group'"=="hsusbwom_kids" {
esttab   using "$RESULTS/bykids_addlhoursoutcomes_indiv_usbwom.tex", keep(SC) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes replace ///
mgroups("Work $>$0 Hours" "Hours if Work" "Hours 35$+$"  "Hours 20$-$34" "Hours 0$-$19"  , ///
 pattern(1 1 1 1 1 1 1 1 1  ) prefix(\multicolumn{@span}{c}{) suffix(}) span  erepeat(\cmidrule(lr){@span})) ///
 mtitles("" "" "" "" "" "" "" "" "")  ///
stats(  ymean p perc N, ///
 labels ( "Mean Y" "P-Value" "\% Effect" "N") fmt( 2 2 2 0) )  nonumbers prefoot("") postfoot("")  label ///
 	varlabels( , blist(SC "\midrule \it{\underline{A: Kids of Any Age}} \\ "))
	eststo clear
	}

 if "`group'"=="hsusbwom_ykids" {
	esttab   using "$RESULTS/bykids_addlhoursoutcomes_indiv_usbwom.tex", keep(SC) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes append ///
	mtitles("" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "") stats(  ymean p perc N, ///
 labels ( "Mean Y" "P-Value" "\% Effect" "N") fmt( 2 2 2 0) ) ///
	nonumbers prefoot("") postfoot("")	prehead("") posthead("")   label ///
 	varlabels( , blist(SC "\midrule \it{\underline{B: Kids Under 5}} \\ "))
	eststo clear
}

if "`group'"=="hsusbwom_nokids" {
	esttab   using "$RESULTS/bykids_addlhoursoutcomes_indiv_usbwom.tex", keep(SC) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes append ///
	mtitles("" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "")  stats(  ymean p perc N, ///
 labels ( "Mean Y" "P-Value" "\% Effect" "N") fmt( 2 2 2 0) )  ///
	nonumbers prefoot("") prehead("") posthead("")  label ///
 	varlabels( , blist(SC "\midrule \it{\underline{C: No Kids }} \\ "))
	eststo clear
	}
	
	
	eststo clear
	
	restore
}


	
	
****************************************************************************************************
****************************************************************************************************
* Table 5 *
****************************************************************************************************
****************************************************************************************************
foreach group in hsusbwom_ykids02 hsusbwom_ykids35 {

preserve

**************************************************
*1. Sample Restrictions 
************************************************** 
keep if `group'==1

**************************************************
*2. Merge in Control and Enforcement Variables at 	cpuma0010 level 
************************************************** 
do "$CODE/acs_mergecontrolscode_feb2022.do"

**************************************************
*3. Set Up Regressions
**************************************************
local yvar " uhrswork lfp  " 
if "`x'"=="lfp" local outcome "Work $>$ 0 Hours"
if "`x'"=="uhrswork" local outcome  "Usual Hours Worked" 

eststo clear  
foreach x of local yvar{ 
if "`x'"=="lfp" local outcome "Work $>$ 0 Hours"
if "`x'"=="uhrswork" local outcome  "Usual Hours Worked"
if "`x'"=="uhrs_c" local outcome  "Usual Hours Worked If Working"
if "`x'"=="uhrs50" local outcome  "Work $>$ 50 Hours"
if "`x'"=="uhrs60" local outcome  "Work $>$ 60 Hours"
if "`x'"=="uhrs50_c" local outcome  "Work $>$ 50 Hours If Working"
if "`x'"=="uhrs60_c" local outcome  "Work $>$ 60 Hours If Working"
qui eststo: reghdfe `x'  $controls $controls_econ $controls_287g $trendsdiff  jail287g task287g SC   [aw=weight], absorb(cpuma0010 year) vce (cluster cpuma0010)  
	estadd ysumm
	test SC =  0  
	estadd scalar p = r(p)
	sum `x' if e(sample)==1  [aw=weight]
	estadd scalar perc = (_b[SC]/r(mean))*100
}

if "`group'"=="hsusbwom_ykids02" {
esttab   using "$RESULTS/bykids_ageyoungestkid_indiv_usbwom.tex", keep(SC) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes replace ///
mgroups("Usual Hours" "Work $>$ 0"  "Usual Hours Worked" , ///
 pattern(1 1 1 1 1 1 1 1 1  ) prefix(\multicolumn{@span}{c}{) suffix(}) span  ) mtitles( "Worked" "Hours" "If Working" )  ///
stats(  ymean p perc N, ///
 labels ( "Mean Y" "P-Value" "\% Effect" "N") fmt( 2 2 2 0) )  nonumbers prefoot("") postfoot("")  label ///
 	varlabels( , blist(SC "\midrule \it{\underline{A: Youngest Kid 0-2}} \\ "))
eststo clear
	}

	if "`group'"=="hsusbwom_ykids35" {
	esttab   using "$RESULTS/bykids_ageyoungestkid_indiv_usbwom.tex", keep(SC) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes append ///
	mtitles("" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "")  stats(  ymean p perc N, ///
 labels ( "Mean Y" "P-Value" "\% Effect" "N") fmt( 2 2 2 0) )  ///
	nonumbers prefoot("") prehead("") posthead("")  label ///
 	varlabels( , blist(SC "\midrule \it{\underline{B: Youngest Kid 3-4}} \\ "))
	eststo clear
	}
	
	restore
	}

	

****************************************************************************************************
****************************************************************************************************
* Table 9 *
****************************************************************************************************
****************************************************************************************************
eststo clear
foreach group in hsusbwom_ykids { 
preserve

**************************************************
*1. Sample Restrictions 
**************************************************  
keep if `group'==1

**************************************************
*2. Merge in Control and Enforcement Variables at 	cpuma0010 level 
************************************************** 
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


local grouplist5 "ls_hsp_fb lsnc_mc_80  lsnc_mx_80 "

gen frac_undoc = (num_ls_hsp_fb/cpuma0010_pop)*100
sum frac_undoc, d

for any undoc mscc_notserious hispanic_nosp centralmx mscc_immigration mscc_dui latin mexico : rename frac_X share_X


**************************************************
*3. Set Up Regressions
************************************************** 
local yvar "  uhrswork "  

foreach x of local yvar{ 

foreach int in  ls_hsp_fb hispanic_nosp centralmx  mexico mscc_notserious  {
sum share_`int'
cap drop share_SC
gen share_SC=share_`int'*SC
if "`int'"=="ls_hsp_fb"  label var share_SC "SC*(LE Hisp FB=All LE)"
if "`int'"=="hispanic_nosp"  label var share_SC "SC*(Share Dep Hispanic)"
if "`int'"=="centralmx"  label var share_SC "SC*(Share Dep CA/MX)"
if "`int'"=="mexico"  label var share_SC "SC*(Share Dep MX)"
if "`int'"=="mscc_notserious"  label var share_SC "SC*(Share Dep Non-Serious Crimes)"

 
qui eststo: reghdfe `x'  $controls_econ $controls_287g $trendsdiff $controls SC share_SC  [aw=weight], absorb(cpuma0010 year) vce (cluster cpuma0010)  
estadd ysumm
	sum share_`int' [aw=weight] if e(sample)==1
	estadd scalar frac = r(mean)
	estadd scalar sd = r(sd)
	estadd scalar b_atmean = _b[SC]+(_b[share_SC]*r(mean))
	estadd scalar b_atmeansd = _b[SC]+(_b[share_SC]*(r(mean)+r(sd)))


	if "`x'"=="uhrswork"  & "`group'"=="hsusbwom_ykids" & "`int'"=="ls_hsp_fb" {
	esttab   using "$RESULTS/bykids_intensity_table_usbwom.tex", keep(SC share_SC) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes replace ///
		mtitles(""  )   stats(  ymean frac sd b_atmean b_atmeansd  N, ///
		labels ( "Mean Y" "Mean Intensity" "SD Intensity" "$\beta$--Mean Int" "$\beta$--1 SD Higher Int" "N") fmt( 2 2 2 2 2 0) ) ///
		nonumbers prefoot("") postfoot("") label ///
 	varlabels( , blist(SC "\midrule \it{\underline{A: Hours Worked}} \\ "))
	eststo clear
	}
	

	
	if "`x'"=="uhrswork"  & "`group'"=="hsusbwom_ykids" & "`int'"=="hispanic_nosp" {
	esttab   using "$RESULTS/bykids_intensity_table_usbwom.tex", keep(SC share_SC) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes append ///
		mtitles(""  )   stats(  ymean frac sd b_atmean b_atmeansd  N, ///
		labels ( "Mean Y" "Mean Intensity" "SD Intensity" "$\beta$--Mean Int" "$\beta$--1 SD Higher Int" "N") fmt( 2 2 2 2 2 0) ) ///
		nonumbers prefoot("") postfoot("") prehead("") posthead("")  label ///
 	varlabels( , blist(SC "\midrule \it{\underline{B: Hours Worked}} \\ "))
	eststo clear
	}
	
	if "`x'"=="uhrswork"  & "`group'"=="hsusbwom_ykids" & "`int'"=="centralmx" {
	esttab   using "$RESULTS/bykids_intensity_table_usbwom.tex", keep(SC share_SC) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes append ///
		mtitles(""  )   stats(  ymean frac sd b_atmean b_atmeansd  N, ///
		labels ( "Mean Y" "Mean Intensity" "SD Intensity" "$\beta$--Mean Int" "$\beta$--1 SD Higher Int" "N") fmt( 2 2 2 2 2 0) ) ///
		nonumbers prefoot("") postfoot("") prehead("") posthead("")  label ///
 	varlabels( , blist(SC "\midrule \it{\underline{C: Hours Worked}} \\ "))
	eststo clear
	}
	
	if "`x'"=="uhrswork"  & "`group'"=="hsusbwom_ykids" & "`int'"=="mexico" {
	esttab   using "$RESULTS/bykids_intensity_table_usbwom.tex", keep(SC share_SC) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes append ///
		mtitles(""  )   stats(  ymean frac sd b_atmean b_atmeansd  N, ///
		labels ( "Mean Y" "Mean Intensity" "SD Intensity" "$\beta$--Mean Int" "$\beta$--1 SD Higher Int" "N") fmt( 2 2 2 2 2 0) ) ///
		nonumbers prefoot("") postfoot("") prehead("") posthead("")  label ///
 	varlabels( , blist(SC "\midrule \it{\underline{D: Hours Worked}} \\ "))
	eststo clear
	}
	
	
	
	if "`x'"=="uhrswork"  & "`group'"=="hsusbwom_ykids" & "`int'"== "mscc_notserious" {
	esttab   using "$RESULTS/bykids_intensity_table_usbwom.tex", keep(SC share_SC) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes append ///
		mtitles("" "" "" "" "" "" "" "") stats(  ymean frac sd b_atmean b_atmeansd  N, ///
		labels ( "Mean Y" "Mean Intensity" "SD Intensity" "$\beta$--Mean Int" "$\beta$--1 SD Higher Int" "N") fmt( 2 2 2 2 2 0) ) ///
		nonumbers prefoot("") prehead("") posthead("")  label ///
 	varlabels( , blist(SC "\midrule \it{\underline{E: Hours Worked}} \\ "))
	eststo clear
	}
	
	}
	}
	restore
	}
	
	

	


****************************************************************************************************
*  Table A3 *
****************************************************************************************************
gen childborn = (fertyr==2)
replace childborn=. if fertyr==0 | fertyr==8
replace childborn = childborn*100
eststo clear
foreach group in  hsusbwom_kids hsusbwom_ykids {

preserve

**************************************************
*1. Sample Restrictions 
************************************************** 
keep if `group'==1

**************************************************
*2. Merge in Control and Enforcement Variables at 	cpuma0010 level 
************************************************** 
do "$CODE/acs_mergecontrolscode_feb2022.do"

**************************************************
*3. Set Up Regressions
**************************************************
local yvar "  childborn" 


global trendsdiff "cit_diff black_diff noncit_diff lfp_diff child5_diff children_diff uhrs50_c_diff uhrs60_c_diff  college_diff master_diff phd_diff wcollege_diff wmaster_diff wphd_diff urate_diff HPIwith2000base_diff"


eststo clear  
foreach x of local yvar{ 
qui eststo: reghdfe `x' $controls_econ $controls_287g $trendsdiff $controls  SC   [aw=weight], absorb(cpuma0010 year  ) vce (cluster cpuma0010)  
	estadd ysumm
	test SC =  0  
	estadd scalar p = r(p)

}
 
if "`group'"=="hsusbwom_kids" {
esttab   using "$RESULTS/fertility_indiv_usbwom.tex", keep(SC ) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes replace ///
mtitles("Had Child in Last 12 Months" "" "" "" "" "" "" "" "" "" "" "" "" "" )  ///
stats(  ymean  N, ///
 labels ( "Mean Y"   "N") fmt( 2  0) ) nonumbers prefoot("") postfoot("") label ///
 	varlabels( , blist(SC "\midrule \it{\underline{A: Any Kids}} \\ "))
eststo clear
	}

if "`group'"=="hsusbwom_ykids" {
	esttab   using "$RESULTS/fertility_indiv_usbwom.tex", keep(SC ) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes append ///
	mtitles("" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "")  stats(  ymean   N, ///
	labels ( "Mean Y"  "N") fmt( 2  0) ) ///
	nonumbers prefoot("") prehead("") posthead("")  label ///
 	varlabels( , blist(SC "\midrule \it{\underline{B: Kids Under 5}} \\ "))
	eststo clear
	}
	
	restore
	}
	
	
	


****************************************************************************************************
****************************************************************************************************
* Table A8  *
****************************************************************************************************
****************************************************************************************************
foreach group in   hsusbwom_ykids {

preserve
drop if year>2014

**************************************************
*1. Sample Restrictions 
************************************************** 
cap drop _merge
 
keep if `group'==1

**************************************************
*2. Merge in Control and Enforcement Variables at 	cpuma0010 level 
************************************************** 
do "$CODE/acs_mergecontrolscode_alttiming.do"


**************************************************
*3. Set Up Regressions
**************************************************
local yvar " uhrswork" 

eststo clear  
foreach x of local yvar{ 
qui eststo: reghdfe `x' $controls_econ $controls_287g $trendsdiff $controls  SC   [aw=weight], absorb(cpuma0010 year) vce (cluster cpuma0010)  
	estadd ysumm
	}
	


	
	if "`group'"=="hsusbwom_ykids" {
	esttab   using "$RESULTS/bykids_timing_indiv_usbwom.tex", keep(SC) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes replace ///
	mgroups("Usual Hours" "Work $>$ 0"   , ///
	 pattern(1 1 1 1 1 1 1 1 1  ) prefix(\multicolumn{@span}{c}{) suffix(}) span  ) mtitles("Worked" "Hours" )  ///
	 stats(  ymean  N, ///
	 labels ( "Mean Y"  "N") fmt( 2  0) ) nonumbers prefoot("") postfoot("")   label ///
		varlabels( , blist(SC "\midrule \it{\underline{A: Kids Under 5, January}} \\ "))
	eststo clear
	}

	for any jail287g task287g SC: rename X  X_jan
	sum jail* task* SC*
	local enfor2 "_frac_lastyr"
	local enfor3 "_frac"
	local enfor4 "_frac_nextyr"
	forvalues c=2/3 { 
	for any jail287g task287g SC: rename X`enfor`c''  X
	label var SC "Secure Communities"
	foreach x of local yvar{ 
	qui eststo: reghdfe `x' $controls_econ $controls_287g $trendsdiff $controls  SC   [aw=weight], absorb(cpuma0010 year) vce (cluster cpuma0010)  
	estadd ysumm
	}
	
	if "`c'"=="2" local panelname "Fraction Last Year"


	
	if "`c'"=="2" & "`group'"=="hsusbwom_ykids" { 
	esttab   using "$RESULTS/bykids_timing_indiv_usbwom.tex", keep(SC) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes append ///
	mtitles("" "" "" "" "" "" "" "")  stats(  ymean   N, ///
	labels ( "Mean Y"  "N") fmt( 2  0) ) ///
	nonumbers  prefoot("") prehead("") posthead("")   label ///
 	varlabels( , blist(SC "\midrule \it{\underline{B: Kids Under 5, `panelname'}} \\ "))
	}
	sum jail* task* SC*
	for any jail287g task287g SC:  rename X X`enfor`c'' 
	sum jail* task* SC*
	eststo clear
	}
restore
}
	

	
	
****************************************************************************************************
****************************************************************************************************
*  CALLAWAY AND SANTANNA ESTIMATION DATA, CODE THEN RUN IN R *
****************************************************************************************************
****************************************************************************************************
foreach group in hsusbwom_ykids  {
preserve
keep if `group'==1

qui do "$CODE/acs_mergecontrolscode_feb2022.do"

gen year_SC = year if SC>0 & SC~=.
bysort cpuma0010: egen min_year_SC=min(year_SC)
tab min_year_SC, m
rename min_year_SC effyear

gen num=1
collapse (rawsum) num (mean) effyear uhrswork $controls $controls_econ $controls_287g $trendsdiff  SC [aw=weight], by(cpuma0010 year) 


save $ACS/analysis_data_CSestimator_`group'.dta, replace
restore
}
	

	
****************************************************************************************************
*  HIGH-SKILL USB MOTHERS: BACON DECOMP *
****************************************************************************************************
pause off 
foreach group in   hsusbwom_ykids { 

preserve

**************************************************
*1. Sample Restrictions 
************************************************** 

 
keep if `group'==1


**************************************************
*2. Merge in Control and Enforcement Variables at 	cpuma0010 level 
************************************************** 
do "$CODE/acs_mergecontrolscode_feb2022.do"

gen num=1

collapse (rawsum) num (mean) uhrswork $controls $controls_econ $controls_287g $trendsdiff  SC [aw=weight], by(cpuma0010 year) 

//gen time constant weights to use with xtreg
gen num05 = num if year==2005
bysort cpuma0010: egen max_num05=max(num05)

**************************************************
*3. Set Up Regressions
**************************************************
xtset cpuma0010 year

// reg with puma and year FE and time-varying weights
xi: reg uhrswork  i.cpuma0010 i.year SC [aw=num]

//xtreg with time constant weights
xtreg uhrswork SC i.year [aw=max_num05], fe 

// xtreg without weights
xtreg uhrswork SC i.year , fe 

// bacon decomp with no controls and no weights, this is the only way ive been able to get the code to run 
bacondecomp uhrswork SC, ddetail

// storngly balance the panel: drop any PUMAs that are unabalanced
bysort cpuma0010: egen count=count(year)
tab count
keep if count==10

// set the T variable =1 in the last year of the panel, this only affects 2 PUMAs who were already very close to 1
replace SC = 1 if year==2014

bacondecomp uhrswork SC, ddetail


restore
}

	
	


	

/***********************************************************/	
/***********************************************************/	
/*** Set Up Long Run Effects Data Set for Analysis ****/

	shell gunzip "$ACS/usa_00017_2001_18.dta" -d "$dir" 
	use "$ACS/usa_00017_2001_18.dta", clear
	shell gzip "$ACS/usa_00017_2001_18.dta" 


keep if year>=2005

tab year  
 
gen migrant=(migrate1d>=24 & migrate1d<90)


* Education categories 
gen college=(educd==101) 
gen master=(educd==114) 
gen phd=(educd==115|educd==116) 
gen low_educ=1 if educd<=61
gen med_educ=1 if educd>61 & educd<101
gen high_educ = 1 if  college==1|master==1|phd==1

gen wcollege=(college==1 & sex==2)
gen wmaster=(master==1 & sex==2)
gen wphd =(phd==1 & sex==2)

* Demographics 
gen age2=age^2 
gen black=race==2 
gen hisp=(hispan>=1 & hispan<=4) 
gen married=(marst==1|marst==2) 
gen child5=nchlt5!=0 
gen children=nchild!=0 
gen cit = 1 if bpl>=1 & bpl<=120 // born in US or US territory 
replace cit = 1 if bpl>120 & citizen==2 // born outside US and US territories, naturalized US citizen
replace cit = 1 if bpl>120 & citizen==1 // born outside US and US territories, but born to US parents (very likely a citizen: https://travel.state.gov/content/travel/en/legal-considerations/us-citizenship-laws-policies/citizenship-child-born-abroad.html)
gen noncit = (citizen==3) // Non-citizens
gen noncit_ls = (citizen==3 & educ<=6) // Low-skill Non-citizens
gen male_noncit_ls=(noncit_ls==1==1 & sex==1)
gen female_noncit_ls=(noncit_ls==1==1 & sex==2)


* Outcomes 
gen     lfp=uhrswork!=0  
gen     uhrs_c=uhrswork if uhrswork!=0 
replace uhrs_c=. if uhrswork==0 
gen     uhrs50=uhrswork>=50 
gen     uhrs60=uhrswork>=60 
gen     uhrs50_c=uhrswork>=50 
replace uhrs50_c=. if uhrswork==0 
gen     uhrs60_c=uhrswork>=60 
replace uhrs60_c=. if uhrswork==0 

gen     uhrs35plus=uhrswork>=35 
gen     uhrs2035=uhrswork>=20 &  uhrswork<35
gen     uhrs20less=  uhrswork<20

gen     weeks_c=wkswork2 if wkswork2!=0 
replace weeks_c=. if wkswork2==0 
 
replace incwage=. if incwage==999999 
replace incwage=. if incwage==999998 

gen selfemp =(classwkr==1)

* Rescale outcome variables to be * 100
for any lfp uhrs50 uhrs60 uhrs50_c uhrs60_c uhrs35plus uhrs2035 uhrs20less selfemp: replace X = X*100


gen weight = perwt 

/** Annual CPI **/
gen annual_cpi = . 
replace annual_cpi = 152.4 if year==1995
replace annual_cpi = 156.9 if year==1996
replace annual_cpi = 160.5 if year==1997
replace annual_cpi = 163 if year==1998
replace annual_cpi = 166.6 if year==1999
replace annual_cpi = 172.2 if year==2000
replace annual_cpi = 177.1 if year==2001
replace annual_cpi = 179.9 if year==2002
replace annual_cpi = 184 if year==2003
replace annual_cpi = 188.9 if year==2004
replace annual_cpi = 195.3 if year==2005
replace annual_cpi = 201.6 if year==2006
replace annual_cpi = 207.342 if year==2007
replace annual_cpi = 215.303 if year==2008
replace annual_cpi = 214.537 if year==2009
replace annual_cpi = 218.056 if year==2010
replace annual_cpi = 224.939 if year==2011
replace annual_cpi = 229.594 if year==2012
replace annual_cpi = 232.957 if year==2013
replace annual_cpi = 236.736 if year==2014
replace annual_cpi = 237.017 if year==2015
replace annual_cpi = 240.007 if year==2016
replace annual_cpi = 245.120 if year==2017
replace annual_cpi = 251.107 if year==2018
replace annual_cpi = annual_cpi /251.107 // 2018 $


* Set Up Controls
global controls "age age2 black married nchlt5 nchild college master phd"
global controls_noedu "age age2 black married child5 children"
global controls_nokids "age age2 black married  college master phd"
global controls_noedu_nokids "age age2 black married  "
global controls_eventstudy "age age2 black married nchlt5 nchild college master phd"
global controls_econ "shift_share_sample shift_share_sample_immig shift_share_sample_hskillw shift_share_sample_hskillm "
global controls_287g "jail287g task287g"
global trendsdiff "cit_diff black_diff noncit_diff lfp_diff child5_diff children_diff uhrs50_c_diff uhrs60_c_diff  college_diff master_diff phd_diff wcollege_diff wmaster_diff wphd_diff urate_diff HPIwith2000base_diff"

* Define Demographic Samples for Regressions
gen hscitwom = 1 if age>=20 & age<64  & sex==2 & (college==1|master==1|phd==1) & cit==1
gen hscitwom_kids = 1 if age>=20 & age<64  & sex==2 & (college==1|master==1|phd==1) & cit==1 & children>0 & children~=.
gen hscitmen = 1 if age>=20 & age<64  & sex==1 & (college==1|master==1|phd==1) & cit==1
gen hscitmen_kids = 1 if age>=20 & age<64  & sex==1 & (college==1|master==1|phd==1) & cit==1 & children>0 & children~=.
gen hsusbwom = 1 if age>=20 & age<64  & sex==2 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56
gen hsusbwom_kids = 1 if age>=20 & age<64  & sex==2 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56 & nchild>0 & nchild~=.
gen hsusbwom_ykids = 1 if age>=20 & age<64  & sex==2 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56 & nchlt5>0 & nchlt5~=.
gen hsusbwom_nokids = 1 if age>=20 & age<64  & sex==2 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56 & nchild==0
gen hsusbwom_ykids02 = 1 if age>=20 & age<64  & sex==2 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56 & yngch>=0 & yngch<=2 & yngch~=.
gen hsusbwom_ykids35 = 1 if age>=20 & age<64  & sex==2 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56 & yngch>=3 & yngch<=5 & yngch~=.
gen hsusbmen = 1 if age>=20 & age<64  & sex==1 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56
gen hsusbmen_kids = 1 if age>=20 & age<64  & sex==1 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56 & nchild>0 & nchild~=.
gen hsusbmen_ykids = 1 if age>=20 & age<64  & sex==1 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56 & nchlt5>0 & nchlt5~=.
gen hsusbmen_nokids = 1 if age>=20 & age<64  & sex==1 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56 & nchild==0
gen hsusbmen_ykids02 = 1 if age>=20 & age<64  & sex==1 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56 & yngch>=0 & yngch<=2 & yngch~=.
gen hsusbmen_ykids35 = 1 if age>=20 & age<64  & sex==1 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56 & yngch>=3 & yngch<=5 & yngch~=.


set more off



	
****************************************************************************************************
****************************************************************************************************
* HIGH SKILLED MOTHERS BY AGE OF YOUNGEST KID CONTROLLING FOR ELIG AT OLDER AGES TOO *
****************************************************************************************************
****************************************************************************************************



foreach group in hsusbwom_kids hsusbmen_kids {
preserve

**************************************************
*1. Sample Restrictions 
************************************************** 

keep if `group'==1


**************************************************
*2a. Merge in Control and Enforcement Variables at PUMA, year of child birth level 
************************************************** 


// year of birth of youngest child and years at different ages
gen byear_yngch = year-yngch 
forvalues n=0/15 {
gen year_yngch_age`n' = byear_yngch+`n' 
}


local q "cpuma0010"
local o "cpuma0010"
// whether enforcement in place in birth year of youngest child
rename year year_temp
rename byear_yngch year
merge m:1 `o' year using $ENFOR/287g_SC_EVerify_1_31_22_cpuma0010.dta
tab year _merge
sum statefip puma if _merge==1
sum statefip puma if _merge==2 & year<=2011 //not merging are pumas that changed codes because of katrina, HI, and VA
drop _merge 
drop *_march *_frac
for any jail287g task287g state287g SC: rename X_jan X_byear


// whether enforcement in place when youngest child ages 0-15
rename year byear_yngch 

forvalues n=0/15 {
rename year_yngch_age`n' year
merge m:1 `o' year using $ENFOR/287g_SC_EVerify_1_31_22_cpuma0010.dta
tab year _merge
sum statefip puma if _merge==1
sum statefip puma if _merge==2 & year<=2011 //not merging are pumas that changed codes because of katrina, HI, and VA
drop _merge 
drop *_march *_frac
for any jail287g task287g state287g SC: rename X_jan X_yngch_age`n'
rename year year_yngch_age`n' 
}


foreach enfor in jail287g task287g state287g SC {
// generate enforcement exposure when youngest kid ages 0-2
gen `enfor'_yngch_age02 =  `enfor'_yngch_age0 + `enfor'_yngch_age1 + `enfor'_yngch_age2

// generate enforcement exposure from age 3 to age at survey
gen `enfor'_yngch_age3survey = `enfor'_yngch_age3 if yngch==3
forvalues n=4/15 {
egen `enfor'_yngch_age3survey`n'= rowtotal(`enfor'_yngch_age3-`enfor'_yngch_age`n') if yngch==`n'
}
forvalues n=4/15 {
replace `enfor'_yngch_age3survey = `enfor'_yngch_age3survey`n'/(yngch-2) if yngch==`n'
}
}

sum *_yngch_age3survey if byear_yngch==2005
sum *_yngch_age3survey if byear_yngch==2010
sum *_yngch_age3survey if byear_yngch==2012

rename year_temp year

tab year
 
 
**************************************************
*2b. Merge in Control and Enforcement Variables at 	cpuma0010 and SURVEY YEAR level 
************************************************** 
do "$CODE/acs_mergecontrolscode_longrun.do"
tab year


**************************************************
*3. Set Up Regressions
************************************************** 

replace incwage=. if incwage>999997
replace incwage =incwage/annual_cpi

gen wkswork3 = 5.5 if wkswork2==1
replace wkswork3 = 21 if wkswork2==2
replace wkswork3 = 32 if wkswork2==3
replace wkswork3 = 43 if wkswork2==4
replace wkswork3 = 48.5 if wkswork2==5
replace wkswork3 = 51 if wkswork2==6

gen logwage=log(incwage/(uhrswork))

gen logwage2=log(incwage/(wkswork3))
gen logwage3=log(incwage/(uhrswork*wkswork3))


tab yngch year if   year>=2005 &  yngch>2 & byear_yngch>=2000  & byear_yngch<=2012 
 
bysort year: sum  SC_yngch_age02 $controls_econ  $trendsdiff $controls  jail287g_yngch_age02   task287g_yngch_age02  
bysort year: sum  SC_yngch_age02 $controls_econ  $trendsdiff $controls  jail287g_yngch_age02   task287g_yngch_age02   if   year>=2005 &  yngch>2 &  yngch<=5 & byear_yngch>=2000  & byear_yngch<=2011
pause 

local yvar "uhrswork      "  //

label var SC_yngch_age02 "SC when Youngest Aged 0$-$2"

local q "cpuma0010"
local o "cpuma0010"

eststo clear  
foreach x of local yvar{ 
 qui eststo: reghdfe `x'     SC_yngch_age02 $controls_econ  $trendsdiff $controls SC_yngch_age3survey jail287g_yngch_age02   task287g_yngch_age02   if   year>=2005 &  yngch>2 &  yngch<=5 & byear_yngch>=2000  & byear_yngch<=2011 [aw=weight], absorb(`q' byear_yngch year) vce (cluster `q') 
	tab year if e(sample)==1
	pause
	estadd ysumm
	test SC_yngch_age02 =  0  
	estadd scalar p = r(p)
	sum year if e(sample)==1
	estadd scalar miny = r(min)
	estadd scalar maxy = r(max)
	sum byear_yngch if e(sample)==1
	estadd scalar minby = r(min)
	estadd scalar maxby = r(max)
}

esttab , keep( SC_yngch_age02) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01)

if "`group'"=="hsusbwom_kids" {
esttab   using "$RESULTS/longrun_usbmothers_contrololder.tex", keep( SC_yngch_age02) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes replace ///
mgroups("Usual Hours" "Work $>$ 0 "  "Log of" , ///
 pattern(1 1 1 1 1 1 1 1 1  ) prefix(\multicolumn{@span}{c}{) suffix(}) span  ) mtitles("Worked" "Hours"  "Hourly Wages" "" "" )  ///
stats(  ymean p miny maxy minby maxby N , ///
 labels ( "Mean Y" "P-Value" "Min Survey Year" "Max Survey Year" "Min Birth Year" "Max Birth Year" "N") fmt( 2 2 0 0 0 0 0) ) ///
 nonumbers prefoot("") postfoot("")  label ///
 	varlabels( , blist(SC_yngch_age02 "\midrule \it{\underline{A: Youngest Child Age 3$-$5 }} \\ "))
eststo clear
}

if "`group'"=="hsusbmen_kids" {
esttab   using "$RESULTS/longrun_usbfathers_contrololder.tex", keep( SC_yngch_age02) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes replace ///
mgroups( "Usual Hours" "Work $>$ 0 " "Log of" , ///
 pattern(1 1 1 1 1 1 1 1 1  ) prefix(\multicolumn{@span}{c}{) suffix(}) span  ) mtitles("Worked" "Hours"  "Hourly Wages" "" "" )  ///
stats(  ymean p miny maxy minby maxby N , ///
 labels ( "Mean Y" "P-Value" "Min Survey Year" "Max Survey Year" "Min Birth Year" "Max Birth Year" "N") fmt( 2 2 0 0 0 0 0) ) ///
 nonumbers prefoot("") postfoot("")  label ///
 	varlabels( , blist(SC_yngch_age02 "\midrule \it{\underline{A: Youngest Child Age 3$-$5 }} \\ "))
eststo clear
}




foreach x of local yvar{ 
 qui eststo: reghdfe `x'     SC_yngch_age02 $controls_econ  $trendsdiff $controls  SC_yngch_age3survey  jail287g_yngch_age02   task287g_yngch_age02   if   year>=2005 &  yngch>5 &  yngch<=7 & byear_yngch>=2000  & byear_yngch<=2011 [aw=weight], absorb(`q' byear_yngch year) vce (cluster `q') 
	estadd ysumm
	test SC_yngch_age02 =  0  
	estadd scalar p = r(p)
	sum year if e(sample)==1
	estadd scalar miny = r(min)
	estadd scalar maxy = r(max)
	sum byear_yngch if e(sample)==1
	estadd scalar minby = r(min)
	estadd scalar maxby = r(max)
}

if "`group'"=="hsusbwom_kids" {
	esttab   using "$RESULTS/longrun_usbmothers_contrololder.tex", keep( SC_yngch_age02) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes append ///
	mtitles("" "" "" "" "" "" "" "")  stats(  ymean p miny maxy minby maxby N , ///
 labels ( "Mean Y" "P-Value" "Min Survey Year" "Max Survey Year" "Min Birth Year" "Max Birth Year" "N") fmt( 2 2 0 0 0 0 0) ) ///
	nonumbers prefoot("") prehead("") posthead("")  label ///
 	varlabels( , blist(SC_yngch_age02 "\midrule \it{\underline{B: Youngest Child Age 6$-$7 }} \\ "))
eststo clear
}

if "`group'"=="hsusbmen_kids" {
	esttab   using "$RESULTS/longrun_usbfathers_contrololder.tex", keep( SC_yngch_age02) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes append ///
	mtitles("" "" "" "" "" "" "" "") stats(  ymean p  miny maxy minby maxby N , ///
 labels ( "Mean Y" "P-Value" "Min Survey Year" "Max Survey Year" "Min Birth Year" "Max Birth Year" "N") fmt( 2 2 0 0 0 0 0) ) ///
	nonumbers prefoot("") prehead("") posthead("")  label ///
 	varlabels( , blist(SC_yngch_age02 "\midrule \it{\underline{B: Youngest Child Age 6$-$7 }} \\ "))
eststo clear
}


restore
}




****************************************************************************************************
****************************************************************************************************
* HIGH SKILLED MOTHERS BY AGE OF YOUNGEST KID CONTROLLING FOR ELIG AT OLDER AGES TOO, DROP MIGRANTS *
****************************************************************************************************
****************************************************************************************************



foreach group in hsusbwom_kids  {
preserve

**************************************************
*1. Sample Restrictions 
************************************************** 

keep if `group'==1


**************************************************
*2a. Merge in Control and Enforcement Variables at PUMA, year of child birth level 
************************************************** 


// year of birth of youngest child and years at different ages
gen byear_yngch = year-yngch 
forvalues n=0/15 {
gen year_yngch_age`n' = byear_yngch+`n' 
}


local q "cpuma0010"
local o "cpuma0010"
// whether enforcement in place in birth year of youngest child
rename year year_temp
rename byear_yngch year
merge m:1  `o' year using $ENFOR/287g_SC_EVerify_1_31_22_cpuma0010.dta 
tab year _merge
sum statefip puma if _merge==1
sum statefip puma if _merge==2 & year<=2011 //not merging are pumas that changed codes because of katrina, HI, and VA
drop _merge 
drop *_march *_frac
for any jail287g task287g state287g SC: rename X_jan X_byear


// whether enforcement in place when youngest child ages 0-15
rename year byear_yngch 

forvalues n=0/15 {
rename year_yngch_age`n' year
merge m:1  `o' year using $ENFOR/287g_SC_EVerify_1_31_22_cpuma0010.dta 
tab year _merge
sum statefip puma if _merge==1
sum statefip puma if _merge==2 & year<=2011 //not merging are pumas that changed codes because of katrina, HI, and VA
drop _merge 
drop *_march *_frac
for any jail287g task287g state287g SC: rename X_jan X_yngch_age`n'
rename year year_yngch_age`n' 
}


foreach enfor in jail287g task287g state287g SC {
// generate enforcement exposure when youngest kid ages 0-2
gen `enfor'_yngch_age02 =  `enfor'_yngch_age0 + `enfor'_yngch_age1 + `enfor'_yngch_age2

// generate enforcement exposure from age 3 to age at survey
gen `enfor'_yngch_age3survey = `enfor'_yngch_age3 if yngch==3
forvalues n=4/15 {
egen `enfor'_yngch_age3survey`n'= rowtotal(`enfor'_yngch_age3-`enfor'_yngch_age`n') if yngch==`n'
}
forvalues n=4/15 {
replace `enfor'_yngch_age3survey = `enfor'_yngch_age3survey`n'/(yngch-2) if yngch==`n'
}
}

sum *_yngch_age3survey if byear_yngch==2005
sum *_yngch_age3survey if byear_yngch==2010
sum *_yngch_age3survey if byear_yngch==2012

rename year_temp year

tab year
 
 
**************************************************
*2b. Merge in Control and Enforcement Variables at 	cpuma0010 and SURVEY YEAR level 
************************************************** 
do "$CODE/acs_mergecontrolscode_longrun.do"
tab year


**************************************************
*3. Set Up Regressions
************************************************** 



tab yngch year if   year>=2005 &  yngch>2 & byear_yngch>=2000  & byear_yngch<=2012 
 
bysort year: sum  SC_yngch_age02 $controls_econ  $trendsdiff $controls  jail287g_yngch_age02   task287g_yngch_age02  
bysort year: sum  SC_yngch_age02 $controls_econ  $trendsdiff $controls  jail287g_yngch_age02   task287g_yngch_age02   if   year>=2005 &  yngch>2 &  yngch<=5 & byear_yngch>=2000  & byear_yngch<=2011
pause 

local yvar "uhrswork      "  //

label var SC_yngch_age02 "SC when Youngest Aged 0$-$2"

local q "cpuma0010"
local o "cpuma0010"

eststo clear  
foreach x of local yvar{ 
 qui eststo: reghdfe `x'     SC_yngch_age02 $controls_econ  $trendsdiff $controls SC_yngch_age3survey jail287g_yngch_age02   task287g_yngch_age02   if   year>=2005 &  yngch>2 &  yngch<=5 & byear_yngch>=2000  & byear_yngch<=2011 [aw=weight], absorb(`q' byear_yngch year) vce (cluster `q') 
	tab year if e(sample)==1
	pause
	estadd ysumm
	test SC_yngch_age02 =  0  
	estadd scalar p = r(p)
	sum year if e(sample)==1
	estadd scalar miny = r(min)
	estadd scalar maxy = r(max)
	sum byear_yngch if e(sample)==1
	estadd scalar minby = r(min)
	estadd scalar maxby = r(max)
}

esttab , keep( SC_yngch_age02) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01)

if "`group'"=="hsusbwom_kids" {
esttab   using "$RESULTS/longrun_usbmothers_contrololder_dropmigrants.tex", keep( SC_yngch_age02) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes replace ///
mgroups("Usual Hours" "Work $>$ 0 "  "Log of" , ///
 pattern(1 1 1 1 1 1 1 1 1  ) prefix(\multicolumn{@span}{c}{) suffix(}) span  ) mtitles("Worked" "Hours"  "Hourly Wages" "" "" )  ///
stats(  ymean p miny maxy minby maxby N , ///
 labels ( "Mean Y" "P-Value" "Min Survey Year" "Max Survey Year" "Min Birth Year" "Max Birth Year" "N") fmt( 2 2 0 0 0 0 0) ) ///
 nonumbers prefoot("") postfoot("")  label ///
 	varlabels( , blist(SC_yngch_age02 "\midrule \it{\underline{A: Youngest Child Age 3$-$5 }} \\ "))
eststo clear
}


eststo clear  
foreach x of local yvar{ 
 qui eststo: reghdfe `x'     SC_yngch_age02 $controls_econ  $trendsdiff $controls SC_yngch_age3survey jail287g_yngch_age02   task287g_yngch_age02   if migrant~=1 &   year>=2005 &  yngch>2 &  yngch<=5 & byear_yngch>=2000  & byear_yngch<=2011 [aw=weight], absorb(`q' byear_yngch year) vce (cluster `q') 
	tab year if e(sample)==1
	pause
	estadd ysumm
	test SC_yngch_age02 =  0  
	estadd scalar p = r(p)
	sum year if e(sample)==1
	estadd scalar miny = r(min)
	estadd scalar maxy = r(max)
	sum byear_yngch if e(sample)==1
	estadd scalar minby = r(min)
	estadd scalar maxby = r(max)
}

esttab , keep( SC_yngch_age02) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01)

if "`group'"=="hsusbwom_kids" {
esttab   using "$RESULTS/longrun_usbmothers_contrololder_dropmigrants.tex", keep( SC_yngch_age02) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes append ///
mgroups("" "" , ///
 pattern(1 1 1 1 1 1 1 1 1  ) prefix(\multicolumn{@span}{c}{) suffix(}) span  ) mtitles("" "" "" )  ///
stats(  ymean p miny maxy minby maxby N , ///
 labels ( "Mean Y" "P-Value" "Min Survey Year" "Max Survey Year" "Min Birth Year" "Max Birth Year" "N") fmt( 2 2 0 0 0 0 0) ) ///
 nonumbers prefoot("") prehead("") posthead("") postfoot("")  label ///
 	varlabels( , blist(SC_yngch_age02 "\midrule \it{\underline{B: Youngest Child Age 3$-$5, Drop Migrants }} \\ "))
eststo clear
}




foreach x of local yvar{ 
 qui eststo: reghdfe `x'     SC_yngch_age02 $controls_econ  $trendsdiff $controls  SC_yngch_age3survey  jail287g_yngch_age02   task287g_yngch_age02   if   year>=2005 &  yngch>5 &  yngch<=7 & byear_yngch>=2000  & byear_yngch<=2011 [aw=weight], absorb(`q' byear_yngch year) vce (cluster `q') 
	estadd ysumm
	test SC_yngch_age02 =  0  
	estadd scalar p = r(p)
	sum year if e(sample)==1
	estadd scalar miny = r(min)
	estadd scalar maxy = r(max)
	sum byear_yngch if e(sample)==1
	estadd scalar minby = r(min)
	estadd scalar maxby = r(max)
}

if "`group'"=="hsusbwom_kids" {
	esttab   using "$RESULTS/longrun_usbmothers_contrololder_dropmigrants.tex", keep( SC_yngch_age02) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes append ///
	mtitles("" "" "" "" "" "" "" "")  stats(  ymean p miny maxy minby maxby N , ///
 labels ( "Mean Y" "P-Value" "Min Survey Year" "Max Survey Year" "Min Birth Year" "Max Birth Year" "N") fmt( 2 2 0 0 0 0 0) ) ///
	nonumbers prefoot("") prehead("") posthead("") postfoot("")  label ///
 	varlabels( , blist(SC_yngch_age02 "\midrule \it{\underline{C: Youngest Child Age 6$-$7 }} \\ "))
eststo clear
}


foreach x of local yvar{ 
 qui eststo: reghdfe `x'     SC_yngch_age02 $controls_econ  $trendsdiff $controls  SC_yngch_age3survey  jail287g_yngch_age02   task287g_yngch_age02   if migrant~=1 &  year>=2005 &  yngch>5 &  yngch<=7 & byear_yngch>=2000  & byear_yngch<=2011 [aw=weight], absorb(`q' byear_yngch year) vce (cluster `q') 
	estadd ysumm
	test SC_yngch_age02 =  0  
	estadd scalar p = r(p)
	sum year if e(sample)==1
	estadd scalar miny = r(min)
	estadd scalar maxy = r(max)
	sum byear_yngch if e(sample)==1
	estadd scalar minby = r(min)
	estadd scalar maxby = r(max)
}

if "`group'"=="hsusbwom_kids" {
	esttab   using "$RESULTS/longrun_usbmothers_contrololder_dropmigrants.tex", keep( SC_yngch_age02) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes append ///
	mtitles("" "" "" "" "" "" "" "")  stats(  ymean p miny maxy minby maxby N , ///
 labels ( "Mean Y" "P-Value" "Min Survey Year" "Max Survey Year" "Min Birth Year" "Max Birth Year" "N") fmt( 2 2 0 0 0 0 0) ) ///
	nonumbers prefoot("") prehead("") posthead("")  label ///
 	varlabels( , blist(SC_yngch_age02 "\midrule \it{\underline{D: Youngest Child Age 6$-$7, Drop Migrants }} \\ "))
eststo clear
}

restore
}



 
