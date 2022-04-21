
clear 
clear matrix 
set mem 500m 
set more off 
clear mata
set matsize 11000
set maxvar 11000
pause on


macro define ACS       "" 
macro define ENFOR     "" 
macro define RESULTS   "" 
macro define CODE      "" 


local o "cpuma0010"

use $ENFOR/detainers_`o'.dta, clear
keep if year ==2002 | year==2005
reshape wide detainers, i(cpuma0010) j(year)
gen change_det = detainers2005-detainers2002
keep change_det cpuma0010
tempfile det
save "`det'", replace

use $ENFOR/287g_SC_EVerify_1_31_22_cpuma0010.dta, clear
gen year_SC = year if SC_jan>0 & SC_jan~=.
bysort  cpuma0010: egen min_year_SC = min(year_SC)
keep min_year_SC  cpuma0010
duplicates drop
tempfile rolloutdates
save "`rolloutdates'", replace

* Use PUMA Char for Trends Data Set
use "$ACS/char_for_trends.dta", clear

merge 1:1   cpuma0010 using "`rolloutdates'"
drop _merge

merge 1:1  cpuma0010  using "`det'"

label var diff_cit_share_0500 "Change \% Citizen"
label var diff_black_share_0500 "Change \% Black"
label var diff_lfp_share_0500 "Change \% Labor Force Part"
label var diff_noncit_share_0500 "Change \% Non-Citizen"
label var diff_child5_share_0500 "Change \% with Children Under 6"
label var diff_children_share_0500 "Change \% with Children"
label var diff_uhrs50_c_share_0500 "Change \% Work $>50$ Hours if Work"
label var diff_uhrs60_c_share_0500 "Change \%  Work $>60$ Hours if Work"
label var diff_college_share_0500 "Change \% with College "
label var diff_master_share_0500 "Change \% with Masters"
label var diff_phd_share_0500 "Change \% with Ph.D."
label var diff_wcollege_share_0500 "Change \% Women with College"
label var diff_wmaster_share_0500 "Change \% Women with Masters"
label var diff_wphd_share_0500 "Change \% Women with Ph.D."
label var diff_urate "Change Unemployment Rate"
label var diff_HPIwith2000base "Change Housing Prices"
label var change_det "Change Detentions"


****************************************************************************************************
* Table 2 *
****************************************************************************************************
eststo clear
qui eststo: reg   min_year_SC diff_cit_share_0500 diff_black_share_0500 diff_lfp_share_0500 diff_noncit_share_0500 diff_child5_share_0500 diff_children_share_0500 diff_uhrs50_c_share_0500 diff_uhrs60_c_share_0500 diff_college_share_0500 diff_master_share_0500 diff_phd_share_0500 diff_wcollege_share_0500 diff_wmaster_share_0500 diff_wphd_share_0500 diff_urate diff_HPIwith2000base change_det
estadd ysumm
test diff_cit_share_0500 diff_black_share_0500 diff_lfp_share_0500 diff_noncit_share_0500 diff_child5_share_0500 diff_children_share_0500 diff_uhrs50_c_share_0500 diff_uhrs60_c_share_0500 diff_college_share_0500 diff_master_share_0500 diff_phd_share_0500 diff_wcollege_share_0500 diff_wmaster_share_0500 diff_wphd_share_0500 diff_urate diff_HPIwith2000base change_det
estadd scalar p_joint = r(p)
esttab, keep(diff_cit_share_0500 diff_black_share_0500 diff_lfp_share_0500 diff_noncit_share_0500 diff_child5_share_0500 diff_children_share_0500 diff_uhrs50_c_share_0500 diff_uhrs60_c_share_0500 diff_college_share_0500 diff_master_share_0500 diff_phd_share_0500 diff_wcollege_share_0500 diff_wmaster_share_0500 diff_wphd_share_0500 diff_urate diff_HPIwith2000base change_det) se stats(ymean r2)
esttab   using "$RESULTS/predict_rollout.tex", keep( diff_cit_share_0500 diff_black_share_0500 diff_lfp_share_0500 diff_noncit_share_0500 diff_child5_share_0500 diff_children_share_0500 diff_uhrs50_c_share_0500 diff_uhrs60_c_share_0500 diff_college_share_0500 diff_master_share_0500 diff_phd_share_0500 diff_wcollege_share_0500 diff_wmaster_share_0500 diff_wphd_share_0500 diff_urate diff_HPIwith2000base change_det) se(3) b(3) star(* 0.10 ** 0.05 *** 0.01) nonum  nonotes replace ///
 mtitles(""  )  ///
stats(  ymean r2 p_joint N , ///
 labels ( "Mean Y" "R-Squared" "F Joint Sig" "N") fmt( 2 2 2 0) ) nonumbers label

summ diff_cit_share_0500 diff_black_share_0500 diff_lfp_share_0500 diff_noncit_share_0500 diff_child5_share_0500 diff_children_share_0500 diff_uhrs50_c_share_0500 diff_uhrs60_c_share_0500 diff_college_share_0500 diff_master_share_0500 diff_phd_share_0500 diff_wcollege_share_0500 diff_wmaster_share_0500 diff_wphd_share_0500 diff_urate diff_HPIwith2000base change_det











































