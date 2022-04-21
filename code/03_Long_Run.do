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
* Table 12 and A7 *
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

rename year_temp year

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

local yvar "uhrswork      "  

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
* Table A6 *
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



 
