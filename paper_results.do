**ssc install rwolf
use "data/survey/survey_stata.dta", replace

/* Labels */
label variable baseline "Baseline"
label variable T1 "Eliminate evasion of the rich"
label variable T2 "Eliminate evasion of the poor"

label variable female "Female"
label variable employed "Employed"
label variable university "College education"
label variable social_security "Access to social security"
label variable indiginous_parent "Indigenous parents"
label variable low_education_parents "Parent with middle-school education or lower"
label variable age "Age"
label variable p13_4 "Perceived tax rate (rich households)"
label variable pre_evasion_ricos "Perceived evasion rate (rich households)"
label variable p13_2 "Perceived tax rate (poor households)"
label variable pre_evasion_pobres "Perceived evasion rate (poor households)"
label variable poverty_index_uw "Beliefs about poverty (index)"
label variable inequality_index_uw "Preoccupation about inequality (index)"
label variable trust_index_uw "Trust in government and satisfaction with public services (index)"
label variable hh_index_uw "Neighborhood quality (index)"
label variable wealth_index_uw "Wealth (index)"
label variable mobilidad "Social mobility"

/***********************************/
/* Table 1: Descriptive statistics */
/***********************************/

putexcel set "tables/table1.xlsx", replace
putexcel A1= "Variable"
putexcel B1= "Total"
putexcel C1= "Baseline"
putexcel D1= "Eliminate evasion of the rich"
putexcel E1= "Eliminate evasion of the poor"
putexcel F1= "p-value"

putexcel A2 = "Number of observations"
quietly sum female 
putexcel B2 = `r(N)'
quietly sum female if celda == 1
putexcel C2 = `r(N)'
quietly sum female if celda == 2
putexcel D2 = `r(N)'
quietly sum female if celda == 3
putexcel E2 = `r(N)'

local i = 3

foreach var in female employed university social_security indiginous_parent low_education_parents age p13_4 pre_evasion_ricos p13_2 pre_evasion_pobres poverty_index_uw inequality_index_uw trust_index_uw hh_index_uw wealth_index_uw{

local varlabel : var label `var'	
putexcel A`i'="`varlabel'"

quietly sum `var'
local meanx = string(`r(mean)',"%9.2f")
local sdx = string((`r(sd)'/sqrt(`r(N)')),"%9.2f")
putexcel B`i'= "`meanx'[`sdx']"

forvalues treatment = 1(1)3{
	quietly sum `var' if celda == `treatment'
	local meanx = string(`r(mean)',"%9.2f")
	local sdx = string((`r(sd)'/sqrt(`r(N)')),"%9.2f")
	
	if `treatment' == 1{
		putexcel C`i'= "`meanx'[`sdx']"
	}
	
	else if `treatment' == 2{
		putexcel D`i'= "`meanx'[`sdx']"
	}
	
	else if `treatment' == 3{
		putexcel E`i'= "`meanx'[`sdx']"
	}
}
quietly reg `var' baseline T1 T2, robust nocons
quietly test baseline=T1=T2
local t = string(`r(p)', "%9.3f")
putexcel F`i'="`t'"
local i `++i'
}
putexcel close

/* Change in perceptions */
putexcel set "tables/change_perceptions.xlsx", replace
putexcel A1= "Variable"
putexcel B1= "Mean change"
putexcel C1= "p-value"

* Tax rates
ttest p20 == p13_4
local delta_mu = `r(mu_1)' - `r(mu_2)'

putexcel A2= "Perceived effective tax rate (rich households)"
putexcel B2 = `delta_mu'
putexcel C2 = `r(p)'

* Evasion
ttest p21 == p13_1_4
local delta_mu = `r(mu_1)' - `r(mu_2)'

putexcel A3= "Perceived tax evasion (rich households)"
putexcel B3 = `delta_mu'
putexcel C3 = `r(p)'

putexcel close

/*********************************************************/
/* Table 2 : Regression Results and Romano-Wolf p-values */
/*********************************************************/

local controls "wealth_index_uw female age pre_evasion_pobres pre_evasion_ricos trust_index_uw poverty_index_uw inequality_index_uw mobilidad employed"
local controls_no_gender "wealth_index_uw age pre_evasion_pobres pre_evasion_ricos trust_index_uw poverty_index_uw inequality_index_uw mobilidad employed"


/* Main effects */
reg p17 T1 T2 `controls', robust
local p_t1 = 2*ttail(e(df_r),abs(_b[T1]/_se[T1]))
local p_t2 = 2*ttail(e(df_r),abs(_b[T2]/_se[T2]))

* F-Test for equality of coefficients
test T1 T2

outreg2 using tables/table2, replace label ctitle(Main Effects) excel
rwolf p17, indepvar(T1 T2) controls(`controls') method(reg) reps(500) seed(12345) robust verbose
local rw_t1 = e(rw_p17_T1)
local rw_t2 = e(rw_p17_T2)

putexcel set "tables/table2_rw.xlsx", replace
putexcel A1= "Model"
putexcel A2= "Main Effects"
putexcel B1= "T1 p-value"
putexcel B2= `p_t1'
putexcel C1= "T1 Romano-Wolf p-value"
putexcel C2= `rw_t1'
putexcel D1= "T2 p-value"
putexcel D2= `p_t2'
putexcel E1= "T2 Romano-Wolf p-value"
putexcel E2= `rw_t2'

/* Subsamples */
* High Wealth
preserve
quietly sum wealth_index_uw, detail
local mediana = r(p50)
keep if wealth_index_uw > `mediana'

reg p17 T1 T2 `controls', robust
local p_t1 = 2*ttail(e(df_r),abs(_b[T1]/_se[T1]))
local p_t2 = 2*ttail(e(df_r),abs(_b[T2]/_se[T2]))
outreg2 using tables/table2, append label ctitle(High wealth) excel

rwolf p17, indepvar(T1 T2) controls(`controls') method(reg) reps(500) seed(12345) robust verbose
local rw_t1 = e(rw_p17_T1)
local rw_t2 = e(rw_p17_T2)

putexcel A3= "High Wealth"
putexcel B3= `p_t1'
putexcel C3= `rw_t1'
putexcel D3= `p_t2'
putexcel E3= `rw_t2'

restore

* Low Wealth
preserve
quietly sum wealth_index_uw, detail
local mediana = r(p50)
keep if wealth_index_uw <= `mediana'

reg p17 T1 T2 `controls', robust
local p_t1 = 2*ttail(e(df_r),abs(_b[T1]/_se[T1]))
local p_t2 = 2*ttail(e(df_r),abs(_b[T2]/_se[T2]))
outreg2 using tables/table2, append label ctitle(Low wealth) excel

rwolf p17, indepvar(T1 T2) controls(`controls') method(reg) reps(500) seed(12345) robust verbose
local rw_t1 = e(rw_p17_T1)
local rw_t2 = e(rw_p17_T2)

putexcel A4= "Low Wealth"
putexcel B4= `p_t1'
putexcel C4= `rw_t1'
putexcel D4= `p_t2'
putexcel E4= `rw_t2'
restore

* Female
preserve
keep if female == 1

reg p17 T1 T2 `controls_no_gender', robust
local p_t1 = 2*ttail(e(df_r),abs(_b[T1]/_se[T1]))
local p_t2 = 2*ttail(e(df_r),abs(_b[T2]/_se[T2]))
outreg2 using tables/table2, append label ctitle(Female) excel

rwolf p17, indepvar(T1 T2) controls(`controls_no_gender') method(reg) reps(500) seed(12345) robust verbose
local rw_t1 = e(rw_p17_T1)
local rw_t2 = e(rw_p17_T2)

putexcel A5= "Female"
putexcel B5= `p_t1'
putexcel C5= `rw_t1'
putexcel D5= `p_t2'
putexcel E5= `rw_t2'
restore

* Male
preserve
keep if female == 0

reg p17 T1 T2 `controls_no_gender', robust
local p_t1 = 2*ttail(e(df_r),abs(_b[T1]/_se[T1]))
local p_t2 = 2*ttail(e(df_r),abs(_b[T2]/_se[T2]))
outreg2 using tables/table2, append label ctitle(Male) excel

rwolf p17, indepvar(T1 T2) controls(`controls_no_gender') method(reg) reps(500) seed(12345) robust verbose
local rw_t1 = e(rw_p17_T1)
local rw_t2 = e(rw_p17_T2)

putexcel A6= "Male"
putexcel B6= `p_t1'
putexcel C6= `rw_t1'
putexcel D6= `p_t2'
putexcel E6= `rw_t2'
restore
putexcel close




/************/
/* Appendix */
/************/

/*********************************************************************************************/
/* Table A1:  Treatment effects and regression coefficients for the willingness to pay taxes */
/* Using college education and parental                                                      */ 
/*********************************************************************************************/

local controls_2 "wealth_index_uw low_education_parents university female age pre_evasion_pobres pre_evasion_ricos trust_index_uw poverty_index_uw inequality_index_uw mobilidad employed"
local controls_no_gender_2 "wealth_index_uw low_education_parents university age pre_evasion_pobres pre_evasion_ricos trust_index_uw poverty_index_uw inequality_index_uw mobilidad employed"


/* Main effects */
reg p17 T1 T2 `controls_2', robust
local p_t1 = 2*ttail(e(df_r),abs(_b[T1]/_se[T1]))
local p_t2 = 2*ttail(e(df_r),abs(_b[T2]/_se[T2]))

* F-Test for equality of coefficients
test T1 T2

outreg2 using tables/tableA1, replace label ctitle(Main Effects) excel
rwolf p17, indepvar(T1 T2) controls(`controls_2') method(reg) reps(500) seed(12345) robust verbose
local rw_t1 = e(rw_p17_T1)
local rw_t2 = e(rw_p17_T2)

putexcel set "tables/tableA1_rw.xlsx", replace
putexcel A1= "Model"
putexcel A2= "Main Effects"
putexcel B1= "T1 p-value"
putexcel B2= `p_t1'
putexcel C1= "T1 Romano-Wolf p-value"
putexcel C2= `rw_t1'
putexcel D1= "T2 p-value"
putexcel D2= `p_t2'
putexcel E1= "T2 Romano-Wolf p-value"
putexcel E2= `rw_t2'

/* Subsamples */
* High Wealth
preserve
quietly sum wealth_index_uw, detail
local mediana = r(p50)
keep if wealth_index_uw > `mediana'

reg p17 T1 T2 `controls_2', robust
local p_t1 = 2*ttail(e(df_r),abs(_b[T1]/_se[T1]))
local p_t2 = 2*ttail(e(df_r),abs(_b[T2]/_se[T2]))
outreg2 using tables/tableA1, append label ctitle(High wealth) excel

rwolf p17, indepvar(T1 T2) controls(`controls_2') method(reg) reps(500) seed(12345) robust verbose
local rw_t1 = e(rw_p17_T1)
local rw_t2 = e(rw_p17_T2)

putexcel A3= "High Wealth"
putexcel B3= `p_t1'
putexcel C3= `rw_t1'
putexcel D3= `p_t2'
putexcel E3= `rw_t2'

restore

* Low Wealth
preserve
quietly sum wealth_index_uw, detail
local mediana = r(p50)
keep if wealth_index_uw <= `mediana'

reg p17 T1 T2 `controls_2', robust
local p_t1 = 2*ttail(e(df_r),abs(_b[T1]/_se[T1]))
local p_t2 = 2*ttail(e(df_r),abs(_b[T2]/_se[T2]))
outreg2 using tables/tableA1, append label ctitle(Low wealth) excel

rwolf p17, indepvar(T1 T2) controls(`controls_2') method(reg) reps(500) seed(12345) robust verbose
local rw_t1 = e(rw_p17_T1)
local rw_t2 = e(rw_p17_T2)

putexcel A4= "Low Wealth"
putexcel B4= `p_t1'
putexcel C4= `rw_t1'
putexcel D4= `p_t2'
putexcel E4= `rw_t2'
restore

* Female
preserve
keep if female == 1

reg p17 T1 T2 `controls_no_gender_2', robust
local p_t1 = 2*ttail(e(df_r),abs(_b[T1]/_se[T1]))
local p_t2 = 2*ttail(e(df_r),abs(_b[T2]/_se[T2]))
outreg2 using tables/tableA1, append label ctitle(Female) excel

rwolf p17, indepvar(T1 T2) controls(`controls_no_gender_2') method(reg) reps(500) seed(12345) robust verbose
local rw_t1 = e(rw_p17_T1)
local rw_t2 = e(rw_p17_T2)

putexcel A5= "Female"
putexcel B5= `p_t1'
putexcel C5= `rw_t1'
putexcel D5= `p_t2'
putexcel E5= `rw_t2'
restore

* Male
preserve
keep if female == 0

reg p17 T1 T2 `controls_no_gender_2', robust
local p_t1 = 2*ttail(e(df_r),abs(_b[T1]/_se[T1]))
local p_t2 = 2*ttail(e(df_r),abs(_b[T2]/_se[T2]))
outreg2 using tables/tableA1, append label ctitle(Male) excel

rwolf p17, indepvar(T1 T2) controls(`controls_no_gender_2') method(reg) reps(500) seed(12345) robust verbose
local rw_t1 = e(rw_p17_T1)
local rw_t2 = e(rw_p17_T2)

putexcel A6= "Male"
putexcel B6= `p_t1'
putexcel C6= `rw_t1'
putexcel D6= `p_t2'
putexcel E6= `rw_t2'
restore
putexcel close


/*********************************************************************************************/
/* Table A2 : Treatment effects and regression coefficients for the willingness to pay taxes */
/* Using the perception of evasion of the rich measured after the treatment                  */
/*********************************************************************************************/

local controls_3 "wealth_index_uw female age pre_evasion_pobres post_evasion_ricos trust_index_uw poverty_index_uw inequality_index_uw mobilidad employed"
local controls_no_gender_3 "wealth_index_uw age pre_evasion_pobres post_evasion_ricos trust_index_uw poverty_index_uw inequality_index_uw mobilidad employed"


/* Main effects */
reg p17 T1 T2 `controls_3', robust
local p_t1 = 2*ttail(e(df_r),abs(_b[T1]/_se[T1]))
local p_t2 = 2*ttail(e(df_r),abs(_b[T2]/_se[T2]))

* F-Test for equality of coefficients
test T1 T2

outreg2 using tables/tableA2, replace label ctitle(Main Effects) excel
rwolf p17, indepvar(T1 T2) controls(`controls_3') method(reg) reps(500) seed(12345) robust verbose
local rw_t1 = e(rw_p17_T1)
local rw_t2 = e(rw_p17_T2)

putexcel set "tables/tableA2_rw.xlsx", replace
putexcel A1= "Model"
putexcel A2= "Main Effects"
putexcel B1= "T1 p-value"
putexcel B2= `p_t1'
putexcel C1= "T1 Romano-Wolf p-value"
putexcel C2= `rw_t1'
putexcel D1= "T2 p-value"
putexcel D2= `p_t2'
putexcel E1= "T2 Romano-Wolf p-value"
putexcel E2= `rw_t2'

/* Subsamples */
* High Wealth
preserve
quietly sum wealth_index_uw, detail
local mediana = r(p50)
keep if wealth_index_uw > `mediana'

reg p17 T1 T2 `controls_3', robust
local p_t1 = 2*ttail(e(df_r),abs(_b[T1]/_se[T1]))
local p_t2 = 2*ttail(e(df_r),abs(_b[T2]/_se[T2]))
outreg2 using tables/tableA2, append label ctitle(High wealth) excel

rwolf p17, indepvar(T1 T2) controls(`controls_3') method(reg) reps(500) seed(12345) robust verbose
local rw_t1 = e(rw_p17_T1)
local rw_t2 = e(rw_p17_T2)

putexcel A3= "High Wealth"
putexcel B3= `p_t1'
putexcel C3= `rw_t1'
putexcel D3= `p_t2'
putexcel E3= `rw_t2'

restore

* Low Wealth
preserve
quietly sum wealth_index_uw, detail
local mediana = r(p50)
keep if wealth_index_uw <= `mediana'

reg p17 T1 T2 `controls_3', robust
local p_t1 = 2*ttail(e(df_r),abs(_b[T1]/_se[T1]))
local p_t2 = 2*ttail(e(df_r),abs(_b[T2]/_se[T2]))
outreg2 using tables/tableA2, append label ctitle(Low wealth) excel

rwolf p17, indepvar(T1 T2) controls(`controls_3') method(reg) reps(500) seed(12345) robust verbose
local rw_t1 = e(rw_p17_T1)
local rw_t2 = e(rw_p17_T2)

putexcel A4= "Low Wealth"
putexcel B4= `p_t1'
putexcel C4= `rw_t1'
putexcel D4= `p_t2'
putexcel E4= `rw_t2'
restore

* Female
preserve
keep if female == 1

reg p17 T1 T2 `controls_no_gender_3', robust
local p_t1 = 2*ttail(e(df_r),abs(_b[T1]/_se[T1]))
local p_t2 = 2*ttail(e(df_r),abs(_b[T2]/_se[T2]))
outreg2 using tables/tableA2, append label ctitle(Female) excel

rwolf p17, indepvar(T1 T2) controls(`controls_no_gender_3') method(reg) reps(500) seed(12345) robust verbose
local rw_t1 = e(rw_p17_T1)
local rw_t2 = e(rw_p17_T2)

putexcel A5= "Female"
putexcel B5= `p_t1'
putexcel C5= `rw_t1'
putexcel D5= `p_t2'
putexcel E5= `rw_t2'
restore

* Male
preserve
keep if female == 0

reg p17 T1 T2 `controls_no_gender_3', robust
local p_t1 = 2*ttail(e(df_r),abs(_b[T1]/_se[T1]))
local p_t2 = 2*ttail(e(df_r),abs(_b[T2]/_se[T2]))
outreg2 using tables/tableA2, append label ctitle(Male) excel

rwolf p17, indepvar(T1 T2) controls(`controls_no_gender_3') method(reg) reps(500) seed(12345) robust verbose
local rw_t1 = e(rw_p17_T1)
local rw_t2 = e(rw_p17_T2)

putexcel A6= "Male"
putexcel B6= `p_t1'
putexcel C6= `rw_t1'
putexcel D6= `p_t2'
putexcel E6= `rw_t2'
restore
putexcel close