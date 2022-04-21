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
/* DATA CLEANING AND CONSTRUCTION */  
/*********************************************************************/


***********************************
* Create PUMA Charecteristics for Trends
	shell gunzip "$ACS/usa_00008.dta" -d "$dir" 
	use "$ACS/usa_00008.dta", clear
	shell gzip "$ACS/usa_00008.dta" 
	append using "$ACS/usa_00014.dta"
	
cap drop _merge 
keep if age>=20 & age<64 
keep if year==2000 | year==2005

gen cit = 1 if bpl>=1 & bpl<=120 // born in US or US territory 
replace cit = 1 if bpl>120 & citizen==2 // born outside US and US territories, naturalized US citizen
replace cit = 1 if bpl>120 & citizen==1 // born outside US and US territories, but born to US parents (very likely a citizen: https://travel.state.gov/content/travel/en/legal-considerations/us-citizenship-laws-policies/citizenship-child-born-abroad.html)
gen noncit=(citizen==3)
gen all=1

gen weight = perwt 

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

* Control variables  
* Control variables 
gen age2=age^2 
gen black=race==2 
gen married=(marst==1|marst==2) 
gen child5=nchlt5!=0 
gen children=nchild!=0 

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
gen     weeks_c=wkswork2 if wkswork2!=0 
replace weeks_c=. if wkswork2==0 
gen     incwage_c=incwage 
replace incwage_c=. if incwage==999999 
replace incwage_c=. if incwage==999998  

for any lfp uhrs50 uhrs60 uhrs50_c uhrs60_c: replace X = X*100

* Gen shares at the puma year level
sum lfp  uhrswork uhrs_c uhrs50 uhrs60 uhrs50_c uhrs60_c ///
age age2 black married child5 children college master phd ///
cit black noncit child5 children college master phd wcollege wmaster wphd

collapse  (rawsum) weight (sum) all lfp  uhrs50 uhrs60 uhrs50_c uhrs60_c ///
 black married child5 children college master phd ///
cit  noncit wcollege wmaster wphd ///
(mean) uhrswork uhrs_c age age2 [aw=perwt] , by(statefip cpuma0010 year )  

merge m:1 statefip cpuma0010 year using $ENFOR/hpi_ACS_cpuma0010.dta 
tab year _merge // nothing not matched in 2005-2011
keep if _merge==3
drop _merge

merge m:1 statefip cpuma0010 year using $ENFOR/urate_ACS_cpuma0010.dta 
tab year _merge // nothing not matched in 2005-2011
keep if _merge==3 
drop _merge

local var "cit black noncit lfp child5 children uhrs50_c uhrs60_c  college master phd wcollege wmaster wphd"

foreach x of local var{
bysort year statefip cpuma0010: gen `x'_share=`x'/all
gen `x'share_00t=`x'_share if year==2000
bysort statefip cpuma0010: egen `x'_share_00=max(`x'share_00t)
gen `x'share_05t=`x'_share if year==2005
bysort statefip cpuma0010: egen `x'_share_05=max(`x'share_05t)
gen diff_`x'_share_0500 = `x'_share_05-`x'_share_00
}

local econ "urate HPIwith2000base weight"

foreach x of local econ{
gen `x'_00t=`x' if year==2000
bysort statefip cpuma0010: egen `x'_00=max(`x'_00t)
gen `x'_05t=`x' if year==2005
bysort statefip cpuma0010: egen `x'_05=max(`x'_05t)
gen diff_`x' = `x'_05-`x'_00
}

drop year
keep cpuma0010 statefip diff_*  *_00 *_05
duplicates drop

save "$ACS/char_for_trends.dta", replace

***********************************
* Create Event Time Dummies 
preserve
use $ENFOR/287g_SC_EVerify_1_31_22_cpuma0010.dta , clear
sum jail287g* task287g* SC*

gen SC_full = SC_jan==1
gen year_SC_full = year if SC_full==1
bysort cpuma0010: egen min_year_SC_full=min(year_SC_full)
gen SC_start = SC_jan>0 & SC_frac~=.
gen year_SC_start = year if SC_start==1
bysort cpuma0010: egen min_year_SC_start=min(year_SC_start)
gen SC_rampup = min_year_SC_full - min_year_SC_start
sum SC_rampup, d

for any jail287g task287g SC: rename X_jan  X

keep if year>=2005 & year<=2014
		
		local pol "SC"
		gen `pol'_dum = (`pol'>0 & `pol'~=.)

		foreach pol in SC_dum  {
		bysort cpuma0010: egen max_`pol'=max(`pol')
		gen year_`pol'=year if `pol'>0 & `pol'~=.
		bysort cpuma0010: egen min_year_`pol'=min(year_`pol')
		}
			
		local pol "SC_dum"
		sort  cpuma0010 year 
		gen event_flag=0
		replace event_flag  = 1 if `pol'[_n-1]==0 &  `pol'[_n]==1 & cpuma0010[_n-1]==cpuma0010[_n]
		gen year_event_flag=year if event_flag==1
		sort  cpuma0010 year
		bysort cpuma0010 `pol': gen event_time  = `pol' * (_n) 
		bysort cpuma0010 `pol': gen event_time_temp = (_n-_N) 
		replace event_time = event_time_temp if `pol' ==0 & max_`pol'==1
		cap drop  event_flag   event_time_temp  

keep cpuma0010 year	  event_time
bysort event_time: sum cpuma0010
save "$ACS/eventstudy", replace
restore	

	
***********************************
* Create Data Sets with PUMA populations and Immigrant Intensity by PUMA
preserve
	import excel using "$ENFOR/CPUMA0010_summary.xls", clear firstrow
	keep CPUMA0010 PUMA00_Pop00
	rename CPUMA0010 cpuma0010
	rename PUMA00_Pop00 cpuma0010_pop // based on 2000 puma coding population, population of CPUMA0010 in 2000
	destring *, replace
	duplicates drop

	save "$ACS/temp_pop.dta", replace 
	restore

 
	preserve
	use "$ACS/usa_00014.dta", clear
	sum year
	gen pop_all=1
	keep if age>=20 & age<64 
	
	sum pop_all if cpuma0010==1 & year==2010

	collapse  (sum) pop_all ///
	[fw=perwt]  , by(cpuma0010 year)  

	save "$ACS/temp_pop2.dta", replace 
	restore
	
	
	preserve
	use "$ACS/usa_00014.dta", clear
	keep if year==2005
	gen pop_all=1
	keep if age>=20 & age<64 
	gen share_noncit = (citizen==3) // Non-citizens
	gen share_lsnc = (citizen==3 & educ<=6) // Low-skill Non-citizens
	gen share_lsnc_hisp = ((citizen==3 & educ<=6) & (hispan>=1 & hispan<=4))  // share of lsnc that are hisp LSNC
	replace share_lsnc_hisp=. if share_lsnc~=1 
	gen share_pop_lsnc_hisp2 = ((citizen==3 & educ<=6) & (hispan>=1 & hispan<=4)) // share of pop that are hisp LSNC
	gen share_pr_cub_hisp = (hispan==2 | hispan==3) // share of hisp that are cuban or puerto rican
	replace share_pr_cub_hisp=. if hispan==. | hispan==0
	gen share_lsnc_cub = ((citizen==3 & educ<=6) & (bpl==250) ) // share of hisp that are cuban
	replace share_lsnc_cub=. if share_lsnc~=1 
	gen share_lsnc_mcca =(share_lsnc==1 & (bpl==200 | bpl==210)) // share of lsnc that are from MX
	replace share_lsnc_mcca=. if share_lsnc~=1 
	gen share_pop_lsnc_mcca =(share_lsnc==1 & (bpl==200 | bpl==210)) // share of pop that are LSNC from MX
	gen share_lsnc_mx =(share_lsnc==1 & (bpl==200 )) // share of lsnc that are from MX
	replace share_lsnc_mx=. if share_lsnc~=1 
	gen share_pop_lsnc_mx =(share_lsnc==1 & (bpl==200 )) // share of pop that are LSNC from MX
		
	gen share_lsnc_hisp80 = ((citizen==3 & educ<=6) & (hispan>=1 & hispan<=4)  & yrimmig>1980)  // share of lsnc that are hisp LSNC
	replace share_lsnc_hisp80=. if share_lsnc~=1 
	gen share_pop_lsnc_hisp280 = ((citizen==3 & educ<=6) & (hispan>=1 & hispan<=4)  & yrimmig>1980) // share of pop that are hisp LSNC
	gen share_lsnc_mcca80 =(share_lsnc==1 & (bpl==200 | bpl==210)   & yrimmig>1980) // share of lsnc that are from MX
	replace share_lsnc_mcca80=. if share_lsnc~=1 
	gen share_pop_lsnc_mcca80 =(share_lsnc==1 & (bpl==200 | bpl==210)  & yrimmig>1980) // share of pop that are LSNC from MX
	gen share_lsnc_mx80 =(share_lsnc==1 & (bpl==200 )   & yrimmig>1980) // share of lsnc that are from MX
	replace share_lsnc_mx80=. if share_lsnc~=1 
	gen share_pop_lsnc_mx80 =(share_lsnc==1 & (bpl==200 )   & yrimmig>1980) // share of pop that are LSNC from MX
		
	
	gen share_de_o_lsnc_h80 = (((citizen==3 & educ<=6) & (hispan>=1 & hispan<=4)  & yrimmig>1980) & deffect_o==1)
	replace share_de_o_lsnc_h80=. if deffect_o~=1
	gen share_de_o_lsnc_mx80 = (((citizen==3 & educ<=6) & (bpl==200 )  & yrimmig>1980) & deffect_o==1)
	replace share_de_o_lsnc_mx80=. if deffect_o~=1
	gen share_de_o_lsnc_mcca80 = (((citizen==3 & educ<=6) & (bpl==200 | bpl==210)  & yrimmig>1980) & deffect_o==1)
	replace share_de_o_lsnc_mcca80=. if deffect_o~=1
	
	sum share_lsnc_mcca share_lsnc_hisp [aw=perwt] 
	
	gen num_lsnc=share_lsnc
	gen num_lsnc_hisp80=share_lsnc_hisp80
	
	* Alternative definitions by foreign-born 
	gen fgn_born=(bpl>120 & bpl<999)
	gen ls_hsp=(ls==1 & hispan>0 & hispan<9 )
	replace ls_hsp = 0 if hispand==450 // don't count spainards
	gen ls_nohsp=(ls==1 & hispan==0 )
	gen ls_fb=(ls==1 & fgn_born==1)
	gen ls_hsp_fb=(ls_hsp==1 & fgn_born==1)
	gen ls_hsp_fb80=(ls_hsp==1 & fgn_born==1 & yrimmig>1980)
	gen ls_hsp_fb86=(ls_hsp==1 & fgn_born==1 & yrimmig>1986)

	gen ls_hsp_mx86 = (ls_hsp==1 & fgn_born==1 & yrimmig>1986 & bpl==200 )
	gen ls_hsp_mx80 = (ls_hsp==1 & fgn_born==1 & yrimmig>1980 & bpl==200 )

	gen ls_hsp_mc86 = (ls_hsp==1 & fgn_born==1 & yrimmig>1986 & (bpl==200 | bpl==210) )
	gen ls_hsp_mc80 = (ls_hsp==1 & fgn_born==1 & yrimmig>1980 & (bpl==200 | bpl==210) )
		
	mdesc pop_all share_*
	sum share_lsnc citizen educ hispan if share_lsnc_hisp==.
	
	foreach x in ls_hsp_fb ls_hsp_fb80 ls_hsp_mx80 ls_hsp_mc80 {
	gen share_`x' = (ls_hsp_fb)
	replace share_`x' = . if ls_fb~=1
	}
	
	rename ls_hsp_fb num_ls_hsp_fb
	rename ls_hsp_fb80 num_ls_hsp_fb80
	rename ls_hsp_mx80 num_ls_hsp_mx80
	rename ls_hsp_mc80 num_ls_hsp_mc80

	collapse  (mean) pop_all  share_* (sum) num_ls_hsp_fb num_ls_hsp_fb80 ///
	num_ls_hsp_mx80 num_ls_hsp_mc80 num_lsnc num_lsnc_hisp80 ///
	[aw=perwt]  , by(cpuma0010)
	

	save "$ACS/pop_nc.dta", replace 
	restore



***********************************
* Set Up Main Data Set for Analysis 
use $ACS/usa_00014.dta, clear  
cap drop _merge 
* Only in 2005+ can observe pumas
keep if year>=2005
* Only use up to 2014 in analysis of SC
drop if year>2014  
sum year

drop if age<20 | age>=64
drop if perwt<0

* Education categories 
gen college=(educd==101) 
gen master=(educd==114) 
gen phd=(educd==115|educd==116) 
gen somecoll=(educd==65|educd==71|educd==81) 
gen hs=(educd==63|educd==64) 
gen lesshs=(educd<63) 

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

sum age* 

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

* Define Demographic Samples for Regressions
gen all =1
gen hsusbwom = 1 if age>=20 & age<64  & sex==2 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56
gen hsusbwom_kids = 1 if age>=20 & age<64  & sex==2 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56 & nchild>0 & nchild~=.
gen hsusbwom_ykids = 1 if age>=20 & age<64  & sex==2 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56 & nchlt5>0 & nchlt5~=.
gen hsusbwom_nokids = 1 if age>=20 & age<64  & sex==2 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56 & nchild==0
gen hsusbwom_ykids02 = 1 if age>=20 & age<64  & sex==2 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56 & yngch>=0 & yngch<=2 & yngch~=.
gen hsusbmen = 1 if age>=20 & age<64  & sex==1 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56
gen hsusbmen_kids = 1 if age>=20 & age<64  & sex==1 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56 & nchild>0 & nchild~=.
gen hsusbmen_ykids = 1 if age>=20 & age<64  & sex==1 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56 & nchlt5>0 & nchlt5~=.
gen hsusbmen_nokids = 1 if age>=20 & age<64  & sex==1 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56 & nchild==0
gen hsusbmen_ykids02 = 1 if age>=20 & age<64  & sex==1 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56 & yngch>=0 & yngch<=2 & yngch~=.
gen hsusbmen_ykids35 = 1 if age>=20 & age<64  & sex==1 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56 & yngch>=3 & yngch<=4 & yngch~=.
gen hsusbwom_ykids35 = 1 if age>=20 & age<64  & sex==2 & (college==1|master==1|phd==1) & bpl>=1 & bpl<=56 & yngch>=3 & yngch<=4 & yngch~=.

cap drop _merge

save $ACS/analysis_data.dta, replace  





 
