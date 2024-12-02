clear
set more off 
cd "~\Dropbox\IndoChineseDev"

**********************************************************************
//1: First-Stage Regressions Chinese Shares - District
**********************************************************************

use "~\Dropbox\IndoChineseDev_Sheva\Data\Main Versions\V7_district.dta", clear
label variable chineseshares_aszero "Chinese Shares 1930"
label var strait_placebo_diff_log_mddnpls "Dist. to Muria Straits, Recentered"

*2nd order Excluding West Java
eststo clear 
eststo: reg chineseshares_aszero strait_placebo_diff_log_mddnpls i.Pro_FE if Pro_FE>1, cluster(Reg_FE)
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd local wjava No
estadd local controls No

*all controls
*eststo: reg chineseshares_aszero strait_placebo_diff_log_mddnpls altitude ln_dist_coast i.Province_fe#c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia) total_area density total_dry total_sawah Longitude if Province_fe>1, first cluster(Reg_FE)
eststo: reg chineseshares_aszero strait_placebo_diff_log_mddnpls altitude ln_dist_coast i.Pro_FE ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2 ln_total_area ln_total_dry ln_total_sawah Longitude if Pro_FE>1, first cluster(Reg_FE)

estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd local wjava No
estadd local controls Yes

esttab, b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(strait_placebo_diff_log_mddnpls) ///
	stats(fe cluster wjava controls r2 N ymean,  labels(FE Cluster "West Java" Controls R2 Observations Mean) fmt(0 0 0 0 2 0 2)) noabbrev //
esttab using "5_tabfig/_district-level/chineseshare_firststage.tex", tex replace b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(strait_placebo_diff_log_mddnpls) ///
	stats(fe cluster wjava controls r2 N ymean,  labels(FE Cluster "West Java" Controls R2 Observations Mean) fmt(0 0 0 3 0 2)) noabbrev ///
	mtitles("Chinese Shares 1930" "Chinese Shares 1930")

	
	
**********************************************************************
//2: Main Regression Tables (Excludes West Java) Table 2-4
**********************************************************************
//Wealth
use "~\Dropbox\IndoChineseDev_Sheva\Data\Susenas\susenas_village_crosswalk", clear
merge m:1 F1930_Java using "~\Dropbox\IndoChineseDev_Sheva\Data\Statistik Industri\sales_maindata_aggregated_2307.dta", force generate(_merge_susenasvillage)
egen year_FE_susenas = group(sussvyyear)
egen district_FE = group(F1930_Java)
label variable chineseshares_aszero "Chinese Shares 1930"

*2nd order Excluding West Java
eststo clear
ivreg2 equiv_exp (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude i.year_FE_susenas if Pro_FE>1, first cluster(Reg_FE)
gen sample = e(sample)
bysort district_FE: egen count_dist = count(chineseshares_aszero) if sample==1
gen weight = 1/count_dist

eststo: ivreg2 equiv_exp (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude i.year_FE_susenas if Pro_FE>1 [aw=weight], first cluster(Reg_FE)
ereturn list
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
*estadd scalar kplm = e(idp)
estadd local wjava No
estadd scalar ar_pvalue = e(archi2p)
estadd scalar sw_pvalue = e(sstatp)
*estadd local weight Yes

//Log Density
*gen ln_density2000=log(density2000)
use "~\Dropbox\IndoChineseDev_Sheva\Data\Main Versions\V7_district.dta", clear
drop _merge_density2000
merge m:1 F1930_Java using "~\Dropbox\IndoChineseDev_Sheva\Data\Ethnicity Data\density2000_district.dta", force gen(_merge_density2000)
keep if _merge_density2000==3
label variable chineseshares_aszero "Chinese Shares 1930"

*2nd order Excluding West Java
eststo: ivreg2 ln_density2000 (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude if Pro_FE>1, cluster(Reg_FE) first
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
*estadd scalar kplm = e(idp)
estadd local wjava No
estadd scalar ar_pvalue = e(archi2p)
estadd scalar sw_pvalue = e(sstatp)

//Nightlights Growth
use "~\Dropbox\IndoChineseDev_Sheva\Data\Statistik Industri\sales_maindata_aggregated_2307.dta", clear
merge 1:1 F1930_Java using "~\Dropbox\IndoChineseDev_Sheva\Data\Nightlights\nightlightsgrowth_district.dta", force gen(_merge_nlgrowth)
label variable chineseshares_aszero "Chinese Shares 1930"

*2nd order Excluding West Java
eststo: ivreg2 nightlightsgrowth_lev_dist (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude if Pro_FE>1, cluster(Reg_FE) first
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
*estadd scalar kplm = e(idp)
estadd local wjava No
estadd scalar ar_pvalue = e(archi2p)
estadd scalar sw_pvalue = e(sstatp)


esttab, b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat ar_pvalue sw_pvalue,  labels(FE Cluster "West Java" R2 Observations Mean F-stat "Underidentification Test, AR p-value" "Underidentification Test, SW p-value") fmt(0 0 0 3 0 2 2 3 3)) noabbrev //
esttab using "5_tabfig/_village-level/main_w_wjava_tab234.tex", tex replace b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat ar_pvalue sw_pvalue,  labels(FE Cluster "West Java" R2 Observations Mean F-stat "Underidentification Test, AR p-value" "Underidentification Test, SW p-value") fmt(0 0 0 3 0 2 2 3 3)) noabbrev ///
	mtitles("\shortstack{consumption \\ ('00-10)}" "\shortstack{pop. density \\ ('00)}" "\shortstack{$\Delta$ light intensity \\ ('92-10)}")


**********************************************************************
//11: Appendix Regression Tables (Includes West Java) Table 2-4
**********************************************************************
//Wealth
use "~\Dropbox\IndoChineseDev_Sheva\Data\Susenas\susenas_village_crosswalk", clear
merge m:1 F1930_Java using "~\Dropbox\IndoChineseDev_Sheva\Data\Statistik Industri\sales_maindata_aggregated_2307.dta", force generate(_merge_susenasvillage)
egen year_FE_susenas = group(sussvyyear)
egen district_FE = group(F1930_Java)
label variable chineseshares_aszero "Chinese Shares 1930"

*2nd order Keeping West Java
*drop sample count_dist
ivreg2 equiv_exp (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude i.year_FE_susenas if Pro_FE>=1, cluster(Reg_FE)
gen sample = e(sample)
bysort district_FE: egen count_dist = count(chineseshares_aszero) if sample==1
gen weight_wjava = 1/count_dist

eststo clear
eststo: ivreg2 equiv_exp (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude i.year_FE_susenas if Pro_FE>=1 [aw=weight_wjava], first cluster(Reg_FE)
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd scalar kplm = e(idp)
estadd local wjava Yes
estadd scalar ar_pvalue = e(archi2p)
estadd scalar sw_pvalue = e(sstatp)
estadd local weight Yes

	
//Log Density
*gen ln_density2000=log(density2000)
use "~\Dropbox\IndoChineseDev_Sheva\Data\Main Versions\V7_district.dta", clear
drop _merge_density2000
merge m:1 F1930_Java using "~\Dropbox\IndoChineseDev_Sheva\Data\Ethnicity Data\density2000_district.dta", force gen(_merge_density2000)
keep if _merge_density2000==3
label variable chineseshares_aszero "Chinese Shares 1930"

*2nd order Keeping West Java
eststo: ivreg2 ln_density2000 (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude if Pro_FE>=1, first cluster(Reg_FE)
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd scalar kplm = e(idp)
estadd local wjava Yes
estadd scalar ar_pvalue = e(archi2p)
estadd scalar sw_pvalue = e(sstatp)


//Nightlights Growth
use "~\Dropbox\IndoChineseDev_Sheva\Data\Statistik Industri\sales_maindata_aggregated_2307.dta", clear
merge 1:1 F1930_Java using "~\Dropbox\IndoChineseDev_Sheva\Data\Nightlights\nightlightsgrowth_district.dta", force gen(_merge_nlgrowth)
label variable chineseshares_aszero "Chinese Shares 1930"

*2nd order Keeping West Java
eststo: ivreg2 nightlightsgrowth_lev_dist (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah ln_density Longitude if Pro_FE>=1, first cluster(Reg_FE)
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd scalar kplm = e(idp)
estadd local wjava Yes
estadd scalar ar_pvalue = e(archi2p)
estadd scalar sw_pvalue = e(sstatp)

esttab, b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat ar_pvalue sw_pvalue,  labels(FE Cluster "West Java" R2 Observations Mean F-stat "Underidentification Test, AR p-value" "Underidentification Test, SW p-value") fmt(0 0 0 3 0 2 2 3)) noabbrev //
esttab using "5_tabfig/_village-level/appendix_wo_wjava_tab234.tex", tex replace b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat ar_pvalue sw_pvalue,  labels(FE Cluster "West Java" R2 Observations Mean  F-stat "Underidentification Test, AR p-value" "Underidentification Test, SW p-value") fmt(0 0 0 3 0 2 2 3 3)) noabbrev ///
	mtitles("\shortstack{consumption \\ ('00-10)}" "\shortstack{pop. density \\ ('00)}" "\shortstack{$\Delta$ light intensity \\ ('92-10)}")
	
	
	
**********************************************************************
//3: Main Regression Tables (Excludes West Java) Table 5-6
**********************************************************************
// firm sales
use "~\Dropbox\IndoChineseDev_Sheva\Data\Main Versions\V7_district.dta", clear
label variable chineseshares_aszero "Chinese Shares 1930"

eststo clear
*2nd order Excluding West Java
eststo: ivreg2 ln_avg_income (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude if Pro_FE>1, cluster(Reg_FE) first
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd scalar kplm = e(idp)
estadd local wjava No
estadd scalar ar_pvalue = e(archi2p)
estadd scalar sw_pvalue = e(sstatp)


// Financial Sector
use "~\Dropbox\IndoChineseDev_Sheva\Data\Main Versions\V7_district.dta", clear
label variable chineseshares_aszero "Chinese Shares 1930"

*2nd order Excluding West Java
eststo: ivreg2 ln_FinanceICT (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude if Pro_FE>1, cluster(Reg_FE) first
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd scalar kplm = e(iCLUSTERdp)
estadd local wjava No
estadd scalar ar_pvalue = e(archi2p)
estadd scalar sw_pvalue = e(sstatp)


esttab, b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat ar_pvalue sw_pvalue,  labels(FE Cluster "West Java" R2 Observations Mean F-stat "Underidentification Test, AR p-value" "Underidentification Test, SW p-value") fmt(0 0 0 3 0 2 2 3 3)) noabbrev //
esttab using "5_tabfig/_district-level/main_w_wjava_tab56.tex", tex replace b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat ar_pvalue sw_pvalue,  labels(FE Cluster "West Java" R2 Observations Mean  F-stat "Underidentification Test, AR p-value" "Underidentification Test, SW p-value") fmt(0 0 0 3 0 2 2 3 3)) noabbrev ///
	mtitles("\shortstack{firm sales \\ ('86-'06)}" "\shortstack{\# of Finance \\ Firms ('16)}" ///
	)

	

**********************************************************************
//11: 2010 industry participation Shares (Working Productive Aged Males) - Village
**********************************************************************
use "~\Dropbox\IndoChineseDev_Sheva\Data\Main Versions\V7_village.dta", clear

*drop sample count_dist weight
ivreg2 sh_Agriculture_working (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude if Pro_FE>1, cluster(Reg_FE)
gen sample = e(sample)
bysort district_FE: egen count_dist = count(chineseshares_aszero) if sample==1
gen weight = 1/count_dist
label variable chineseshares_aszero "Chinese Shares 1930"

*2nd order Excluding West Java
eststo clear
local varlist Agriculture Manufacturing Services
foreach var of local varlist {
eststo: ivreg2 sh_`var'_working (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude if Pro_FE>1 [aw=weight], first cluster(Reg_FE)
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd scalar kplm = e(idp)
estadd local wjava No
estadd scalar ar_pvalue = e(archi2p)
estadd scalar sw_pvalue = e(sstatp)
*estadd local weight Yes
}

esttab, b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat ar_pvalue sw_pvalue,  labels(FE Cluster "West Java" R2 Observations Mean F-stat "Underidentification Test, AR p-value" "Underidentification Test, SW p-value") fmt(0 0 0 3 0 2 2 3 3)) noabbrev //
esttab using "5_tabfig/_village-level/aggregated_shares_working_indus_2010.tex", tex replace b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat ar_pvalue sw_pvalue,  labels(FE Cluster "West Java" R2 Observations Mean F-stat "Underidentification Test, AR p-value" "Underidentification Test, SW p-value") fmt(0 0 0 3 0 2 2 3 3)) noabbrev ///
	mtitles("Agric. Shares" "Manuf. Shares" "Services Shares") 

	

**********************************************************************
//10: 2010 industry participation Shares (Working Productive Aged Males)- District
**********************************************************************
use "~\Dropbox\IndoChineseDev_Sheva\Data\Main Versions\V7_district.dta", clear
label variable chineseshares_aszero "Chinese Shares 1930"

eststo clear 
*2nd order Excluding West Java
local varlist Agriculture_dist Manufacturing_dist Services_dist
foreach var of local varlist {
eststo: ivreg2 sh_`var'_working (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude if Pro_FE>1, cluster(Reg_FE)
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd scalar kplm = e(idp)
estadd local wjava No
estadd local denominator "Working Only"
}

/*
*2nd order Keeping West Java
eststo: ivreg2 sh_`var'_working (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude if Pro_FE>=1, cluster(Reg_FE)
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd scalar kplm = e(idp)
estadd local wjava Yes
estadd local denominator "Working Only"
}
*/

esttab, b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat kplm denominator,  labels(FE Cluster "West Java" R2 Observations Mean F-stat "Underidentification Test, p-value" Denominator) fmt(0 0 0 3 0 2 2 3)) noabbrev //
esttab using "5_tabfig/_district-level/aggregated_shares_working_indus_2010.tex", tex replace b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat kplm denominator,  labels(FE Cluster "West Java" R2 Observations Mean  F-stat "Underidentification Test, p-value" Denominator) fmt(0 0 0 3 0 2 2 3)) noabbrev ///
	mtitles("\shortstack{Industry \\ Agric. Shares \\ (\% '10)}" "\shortstack{Industry \\ Agric. Shares \\ (\% '10)}" "\shortstack{Industry \\ Manuf. Shares \\ (\% '10)}" "\shortstack{Industry \\ Manuf. Shares \\ (\% '10)}" "\shortstack{Industry \\ Services Shares \\ (\% '10)}" "\shortstack{Industry \\ Services Shares \\ (\% '10)}")	

	
	
	
	
************************************************
//4: Susenas consumption
************************************************
use "~\Dropbox\IndoChineseDev_Sheva\Data\Susenas\susenas_village_crosswalk", clear
merge m:1 F1930_Java using "~\Dropbox\IndoChineseDev_Sheva\Data\Statistik Industri\sales_maindata_aggregated_2307.dta", force generate(_merge_susenasvillage)
egen year_FE_susenas = group(sussvyyear)
egen district_FE = group(F1930_Java)
label variable chineseshares_aszero "Chinese Shares 1930"


*2nd order Excluding West Java
eststo clear
ivreg2 equiv_exp (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude i.year_FE_susenas if Pro_FE>1, first cluster(Reg_FE)
gen sample = e(sample)
bysort district_FE: egen count_dist = count(chineseshares_aszero) if sample==1
gen weight = 1/count_dist

*ivreg2 equiv_exp (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude i.year_FE_susenas if Pro_FE>1 [aw=weight], first cluster(district_FE)
eststo: ivreg2 equiv_exp (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude i.year_FE_susenas if Pro_FE>1 [aw=weight], first cluster(Reg_FE)
ereturn list
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd scalar kplm = e(idp)
estadd local wjava No
estadd scalar ar_pvalue = e(archi2p)
estadd scalar sw_pvalue = e(sstatp)
estadd local weight Yes

*2nd order Keeping West Java
drop sample count_dist
ivreg2 equiv_exp (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude i.year_FE_susenas if Pro_FE>=1, cluster(Reg_FE)
gen sample = e(sample)
bysort district_FE: egen count_dist = count(chineseshares_aszero) if sample==1
gen weight_wjava = 1/count_dist

eststo: ivreg2 equiv_exp (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude i.year_FE_susenas if Pro_FE>=1 [aw=weight_wjava], first cluster(Reg_FE)

estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd scalar kplm = e(idp)
estadd local wjava Yes
estadd scalar ar_pvalue = e(archi2p)
estadd scalar sw_pvalue = e(sstatp)
estadd local weight Yes

esttab, b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat kplm,  labels(FE Cluster "West Java" R2 Observations Mean F-stat "Underidentification Test, p-value") fmt(0 0 0 3 0 2 2 3)) noabbrev //
esttab using "5_tabfig/_village-level/hh-consumption-sus.tex", tex replace b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat kplm ar_pvalue sw_pvalue weight,  labels(FE Cluster "West Java" R2 Observations Mean  F-stat "Underidentification Test, p-value" "Anderson-Rubin, P-Value" "Stock-Wright, P-Value" Weight) fmt(0 0 0 3 0 2 2 3 3 3 0)) noabbrev ///
	mtitles("ln(hh consumption)" "ln(hh consumption)")	
	

************************
//5: 2000 pop density (Log)
************************
use "~\Dropbox\IndoChineseDev_Sheva\Data\Main Versions\V7_district.dta", clear
*gen ln_density2000=log(density2000)
drop _merge_density2000
merge m:1 F1930_Java using "~\Dropbox\IndoChineseDev_Sheva\Data\Ethnicity Data\density2000_district.dta", force gen(_merge_density2000)
keep if _merge_density2000==3
label variable chineseshares_aszero "Chinese Shares 1930"

*2nd order Excluding West Java
eststo clear
eststo: ivreg2 ln_density2000 (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude if Pro_FE>1, cluster(Reg_FE)
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd scalar kplm = e(idp)
estadd local wjava No

*2nd order Keeping West Java
eststo: ivreg2 ln_density2000 (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah ln_density Longitude if Pro_FE>=1, cluster(Reg_FE)
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd scalar kplm = e(idp)
estadd local wjava Yes

esttab, b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat kplm,  labels(FE Cluster "West Java" R2 Observations Mean F-stat "Underidentification Test, p-value") fmt(0 0 0 3 0 2 2 3)) noabbrev //
esttab using "5_tabfig/_district-level/popdensity_p2000.tex", tex replace b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat kplm,  labels(FE Cluster "West Java" R2 Observations Mean  F-stat "Underidentification Test, p-value") fmt(0 0 0 3 0 2 2 3)) noabbrev ///
	mtitles("\shortstack{ln(pop. density) \\ ('00)}" "\shortstack{ln(pop. density) \\ ('00)}" ///
	)
		
	
************************************************
//6: Nightlights Growth (District) 2010-1992
************************************************
use "~\Dropbox\IndoChineseDev_Sheva\Data\Statistik Industri\sales_maindata_aggregated_2307.dta", clear
merge 1:1 F1930_Java using "~\Dropbox\IndoChineseDev_Sheva\Data\Nightlights\nightlightsgrowth_district.dta", force gen(_merge_nlgrowth)
label variable chineseshares_aszero "Chinese Shares 1930"

*2nd order Excluding West Java
eststo clear
eststo: ivreg2 nightlightsgrowth_lev_dist (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude if Pro_FE>1, cluster(Reg_FE)
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd scalar kplm = e(idp)
estadd local wjava No

*2nd order Keeping West Java
eststo: ivreg2 nightlightsgrowth_lev_dist (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah ln_density Longitude if Pro_FE>=1, cluster(Reg_FE)
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd scalar kplm = e(idp)
estadd local wjava Yes


esttab, b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat kplm,  labels(FE Cluster "West Java" R2 Observations Mean F-stat "Underidentification Test, p-value") fmt(0 0 0 3 0 2 2 3)) noabbrev //
esttab using "5_tabfig/_district-level/nightlights.tex", tex replace b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat kplm,  labels(FE Cluster "West Java" R2 Observations Mean  F-stat "Underidentification Test, p-value") fmt(0 0 0 3 0 2 2 3)) noabbrev ///
	mtitles("\shortstack{$\Delta$ light intensity \\ ('92-10)}" "\shortstack{$\Delta$ light intensity \\ ('92-10)}")
	
	
************************
//7: average firm sales 
************************
use "~\Dropbox\IndoChineseDev_Sheva\Data\Main Versions\V7_district.dta", clear
label variable chineseshares_aszero "Chinese Shares 1930"

eststo clear
*2nd order Excluding West Java
eststo: ivreg2 ln_avg_income (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude if Pro_FE>1, cluster(Reg_FE)
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd scalar kplm = e(idp)
estadd local wjava No

*2nd order Keeping West Java
eststo: ivreg2 ln_avg_income (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah ln_density Longitude if Pro_FE>=1, cluster(Reg_FE)
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd scalar kplm = e(idp)
estadd local wjava Yes

esttab, b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat kplm,  labels(FE Cluster "West Java" R2 Observations Mean F-stat "Underidentification Test, p-value") fmt(0 0 0 3 0 2 2 3)) noabbrev //
esttab using "5_tabfig/_district-level/sales_si.tex", tex replace b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat kplm,  labels(FE Cluster "West Java" R2 Observations Mean  F-stat "Underidentification Test, p-value") fmt(0 0 0 3 0 2 2 3)) noabbrev ///
	mtitles("\shortstack{ln(firm sales) \\ ('86-'06)}" "\shortstack{ln(firm sales) \\ ('86-'06)}" ///
	)
	
	
**********************************************************************
//8: Log of financial industry sector (from 2016 economic census)
**********************************************************************
use "~\Dropbox\IndoChineseDev_Sheva\Data\Main Versions\V7_district.dta", clear
label variable chineseshares_aszero "Chinese Shares 1930"

eststo clear 
*2nd order Excluding West Java
eststo: ivreg2 ln_FinanceICT (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude if Pro_FE>1, cluster(Reg_FE)
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd scalar kplm = e(iCLUSTERdp)
estadd local wjava No

*2nd order Keeping West Java
eststo: ivreg2 ln_FinanceICT (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah ln_density Longitude if Pro_FE>=1, cluster(Reg_FE)
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd scalar kplm = e(idp)
estadd local wjava Yes

esttab, b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat kplm,  labels(FE Cluster "West Java" R2 Observations Mean F-stat "Underidentification Test, p-value") fmt(0 0 0 3 0 2 2 3)) noabbrev //
esttab using "5_tabfig/_district-level/ln_FinanceICT.tex", tex replace b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat kplm,  labels(FE Cluster "West Java" R2 Observations Mean  F-stat "Underidentification Test, p-value") fmt(0 0 0 3 0 2 2 3)) noabbrev ///
	mtitles("\shortstack{ln(\# Finance \\ Firms \\ ('16)}" "\shortstack{ln(\# Finance \\ Firms \\ ('16)}" ///
	)
	

	
**********************************************************************
//10: Each 2010 industry participation - Village (Specific Industries)
**********************************************************************
use "~\Dropbox\IndoChineseDev_Sheva\Data\Main Versions\V7_village.dta", clear
label variable chineseshares_aszero "Chinese Shares 1930"

*2nd order Excluding West Java
replace EduHealthSoc = industry16+industry17+industry18

eststo clear 
local varlist industry11 industry12 industry13 industry14 industry15 EduHealthSoc
foreach var of local varlist {
eststo: ivreg2 `var' (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah Longitude if Pro_FE>1, cluster(Reg_FE)
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd scalar kplm = e(idp)
estadd local wjava No
}
ereturn list

esttab, b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat kplm,  labels(FE Cluster "West Java" R2 Observations Mean F-stat "Underidentification Test, p-value") fmt(0 0 0 3 0 2 2 3)) noabbrev //
esttab using "5_tabfig/_village-level/each_indus_2010_service.tex", tex replace b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat kplm,  labels(FE Cluster "West Java" R2 Observations Mean  F-stat "\ Underidentification Test, \\  p-value") fmt(0 0 0 3 0 2 2 3)) noabbrev ///
	mtitles("Trade" "FoodAccomm" "Transport" "ICT" "Finance" "EduHealthSoc")
	
	
**********************************************************************
//12: Appendix Regression Tables (Includes West Java) Table 5-6
**********************************************************************
// Firm sales
use "~\Dropbox\IndoChineseDev_Sheva\Data\Main Versions\V7_district.dta", clear
label variable chineseshares_aszero "Chinese Shares 1930"

eststo clear
*2nd order Keeping West Java
eststo: ivreg2 ln_avg_income (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah ln_density Longitude if Pro_FE>=1, cluster(Reg_FE)
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd scalar kplm = e(idp)
estadd local wjava Yes


// Financial Sector
use "~\Dropbox\IndoChineseDev_Sheva\Data\Main Versions\V7_district.dta", clear
label variable chineseshares_aszero "Chinese Shares 1930"

*2nd order Keeping West Java
eststo: ivreg2 ln_FinanceICT (chineseshares_aszero=strait_placebo_diff_log_mddnpls) altitude ln_dist_coast i.Pro_FE c.(ln_dist_batavia ln_dist_semarang ln_dist_tuban ln_dist_gresik ln_dist_trowulan ln_dist_soerabaia ln_dist_batavia2 ln_dist_semarang2 ln_dist_tuban2 ln_dist_gresik2 ln_dist_trowulan2 ln_dist_soerabaia2) ln_total_area ln_total_dry ln_total_sawah ln_density Longitude if Pro_FE>=1, cluster(Reg_FE)
estadd ysumm 
estadd local fe Province
estadd local cluster Regency
estadd scalar fstat = e(widstat)
estadd scalar kplm = e(idp)
estadd local wjava Yes


esttab, b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat kplm,  labels(FE Cluster "West Java" R2 Observations Mean F-stat "Underidentification Test, p-value") fmt(0 0 0 3 0 2 2 3)) noabbrev //
esttab using "5_tabfig/_district-level/appendix_wo_wjava_tab56.tex", tex replace b(3) se(3) star(* 0.1 ** 0.05 *** 0.01)  label nonote nobase ///
	keep(chineseshares_aszero) ///
	stats(fe cluster wjava r2 N ymean fstat kplm,  labels(FE Cluster "West Java" R2 Observations Mean  F-stat "Underidentification Test, p-value") fmt(0 0 0 3 0 2 2 3)) noabbrev ///
	mtitles("\shortstack{ln(firm sales) \\ ('86-'06)}" "\shortstack{ln(\# Finance \\ Firms \\ ('16)}" ///
	)

