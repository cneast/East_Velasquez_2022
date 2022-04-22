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


/*** Main Data Set for Analysis of Mechanisms ****/

use $ACS/usa_00014.dta, clear  

drop if year>2014
keep if year>=2005  

drop if perwt<0
gen fgn_born=(bpl>120 & bpl<999)
bysort year sample serial: egen max_fb=max(fgn_born)

drop if age<20 | age>=64

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

replace hispan=0 if hispand==450 // don't count spainards

gen puerto_rico=(bpl==110)
gen mc = (bpl==200 | bpl==210 )

gen arrival80 = (yrimmig>1980)
gen ls =( educ<6)
gen hs =( educ>6) if educ!=.

* Definitions by foreign-born 

gen ls_hsp=(ls==1 & hispan>0 & hispan<9 )
replace ls_hsp = 0 if hispand==450 // don't count spainards

gen hs_hsp=(hs==1 & hispan>0 & hispan<9 )
replace hs_hsp = 0 if hispand==450 // don't count spainards

gen ls_nohsp=(ls==1 & hispan==0 )
gen ls_hsp_fb=(ls_hsp==1 & fgn_born==1)
gen hs_hsp_fb=(hs_hsp==1 & fgn_born==1)

gen ls_hsp_fb80=(ls_hsp==1 & fgn_born==1 & yrimmig>1980)
gen ls_hsp_fb86=(ls_hsp==1 & fgn_born==1 & yrimmig>1986)

gen ls_hsp_mx = (ls_hsp==1 & fgn_born==1  & bpl==200 )
gen ls_hsp_mc = (ls_hsp==1 & fgn_born==1 &  (bpl==200 | bpl==210) )
gen ls_hsp_ca = (ls_hsp==1 & fgn_born==1 &  (bpl==210) )

gen ls_hsp_mx86 = (ls_hsp==1 & fgn_born==1 & yrimmig>1986 & bpl==200 )
gen ls_hsp_mx80 = (ls_hsp==1 & fgn_born==1 & yrimmig>1980 & bpl==200 )

gen ls_hsp_mc86 = (ls_hsp==1 & fgn_born==1 & yrimmig>1986 & (bpl==200 | bpl==210) )
gen ls_hsp_mc80 = (ls_hsp==1 & fgn_born==1 & yrimmig>1980 & (bpl==200 | bpl==210) )


gen ls_hsp_nfb=(ls_hsp==1 & fgn_born==0)
gen ls_nfb=(ls==1 & fgn_born==0)
gen hs_nfb=(hs==1 & fgn_born==0)

gen ls_nohsp_nfb=(ls_nohsp==1 & fgn_born==0)

gen usb=(fgn_born==0) if fgn_born!=.
gen usb_nonhsp=(usb==1 & hispan==0 )
gen usb_hsp=(usb==1 & hispan==1 )
gen usb_hsp_fbrel=(usb_hsp==1 & max_fb==1)
gen usb_hsp_nofbrel=(usb_hsp==1 & max_fb==0 )


* Outcomes 
gen     lfp=uhrswork!=0  
gen     uhrs_c=uhrswork if uhrswork!=0 
replace uhrs_c=. if uhrswork==0 


* HH Services definition Based on OCC1990
*468	Child care workers
*405	Housekeepers, maids, butlers, stewards, and lodging quarters cleaners
gen deffect_o=( occ1990==468)
replace deffect_o=1 if  occ1990==405


* Rescale outcome variables to be * 100
for any lfp : replace X = X*100
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


gen all=1 


foreach var in hs hs_hsp_fb hs_nfb ls_hsp_mx ls_hsp_ca ls_hsp_mc  usb usb_nonhsp usb_hsp   ls_hsp_fb80 ls_hsp_fb86 ls_nfb ls_nohsp_nfb all    ls_hsp_mx86 ls_hsp_mx80 ls_hsp_mc86 ls_hsp_mc80   ls_hsp ls_nohsp ls_hsp_fb ls_hsp_nfb ls                  { 
gen hhs_`var' = (deffect_o==1 & `var'==1)
gen hhs_`var'_w =  hhs_`var' if sex==2

}
  
save $ACS/data_mechanisms.dta, replace  

