local q "cpuma0010"
local o "cpuma0010"


merge m:1 statefip `o' year using $ENFOR/final_bartik_acs_`o'_20002018.dta 
tab year _merge
sum statefip puma if _merge==1
sum statefip puma if _merge==2 & year<=2011 // only thing not merging are pumas that changed codes because of katrina
drop _merge  

merge m:1 statefip `o' year using $ENFOR/hpi_ACS_`o'.dta 
tab year _merge // nothing not matched in 2005-2011
drop _merge


merge m:1 `o' year using $ENFOR/287g_SC_EVerify_1_31_22_cpuma0010.dta
tab year _merge
sum statefip puma if _merge==1
sum statefip puma if _merge==2 & year<=2011 //not merging are pumas that changed codes because of katrina, HI, and VA
drop _merge  

sum jail287g* task287g* SC*

for any jail287g task287g SC: rename X_jan  X

label var jail287g "Jail 287(g)"
label var task287g "Task 287(g)"
label var SC "Secure Communities"
 

merge m:1 cpuma0010 using "$ACS/char_for_trends.dta" 
tab year _merge
sum statefip puma if _merge==1
sum statefip puma if _merge==2 & year<=2011 //not merging are pumas that changed codes because of katrina, HI, and VA
keep if _merge==3 
drop _merge 

local var "cit black noncit lfp child5 children uhrs50_c uhrs60_c  college master phd wcollege wmaster wphd"

foreach x of local var{
gen `x'_diff=diff_`x'_share_0500*year
}
 
local econ "urate HPIwith2000base weight"

foreach x of local econ{
gen `x'_diff=diff_`x'*year
}
