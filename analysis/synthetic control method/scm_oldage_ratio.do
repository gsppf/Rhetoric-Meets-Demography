// ============================================================================
// AUTHOR:      GIUSEPPE FAÈ
// CREATED:     JUNE 16, 2025
// MODIFIED:    
// DESCRIPTION: Synthetic Control Method (SCM) for Old Age Dependency Ratio
// ============================================================================

// -----------------------------------------------------------------------------
// 0. Setup
// -----------------------------------------------------------------------------

// * Uncomment below to install necessary packages
* ssc install allsynth, replace
* ssc install distinct, replace
* ssc install elasticregress, replace
* ssc install texsave, replace

use ${data_clean}/populist_demographics.dta, clear
tsset cid year
keep if year >= 1950

// -----------------------------------------------------------------------------
// 1. Define Outcome Variables
// -----------------------------------------------------------------------------

gen lold    = log(oldage_ratio)            // Log Total oldility Rate
gen lrgdppc = log(fstgdp)              // Log Real GDP per Capita

// -----------------------------------------------------------------------------
// 2. Identify First Populist Takeover (Any, Left, Right)
// -----------------------------------------------------------------------------

// --- a. Any Populist Takeover ---
bys cid (year): gen cum_atake     = sum(atakeover)
bys cid:        egen tot_atake   = total(atakeover)
gen             first_atakeover  = (atakeover == 1 & cum_atake == 1)

levelsof cid if first_atakeover, local(tr_cids_any)
local tr_years_any
foreach c of local tr_cids_any {
    quietly summarize year if cid == `c' & first_atakeover, meanonly
    local tr_years_any `tr_years_any' `r(min)'
}
display "Any-pop cids:  `tr_cids_any'"
display "Any-pop years: `tr_years_any'"

// --- b. Left-Wing Populist Takeover ---
bys cid (year): gen  cum_ltake   = sum(ltakeover)
bys cid:        egen tot_ltake   = total(ltakeover)
gen             first_ltakeover  = (ltakeover == 1 & cum_ltake == 1)

levelsof cid if first_ltakeover, local(tr_cids_left)
local tr_years_left
foreach c of local tr_cids_left {
    quietly summarize year if cid == `c' & first_ltakeover, meanonly
    local tr_years_left `tr_years_left' `r(min)'
}
display "Left-pop cids:  `tr_cids_left'"
display "Left-pop years: `tr_years_left'"

// --- c. Right-Wing Populist Takeover ---
bys cid (year): gen  cum_rtake   = sum(rtakeover)
bys cid:        egen tot_rtake   = total(rtakeover)
gen             first_rtakeover  = (rtakeover == 1 & cum_rtake == 1)

levelsof cid if first_rtakeover, local(tr_cids_right)
local tr_years_right
foreach c of local tr_cids_right {
    quietly summarize year if cid == `c' & first_rtakeover, meanonly
    local tr_years_right `tr_years_right' `r(min)'
}
display "Right-pop cids:  `tr_cids_right'"
display "Right-pop years: `tr_years_right'"

// -----------------------------------------------------------------------------
// 3. Build Predictor Variables
// -----------------------------------------------------------------------------

// --- a. Lags of Outcome ---
foreach i in 1 2 3 4 5 10 15 {
    gen L`i'_lold = L`i'.lold
}

// --- b. Lags of Controls ---
foreach v in pop_jul med_age migration_rate lrgdppc bankcrisis institutions {
    gen L1_`v' = L.`v'
}

// --- c. Local Macro for Predictors ---
#delimit ;
local preds 
    L15_lold
    L10_lold
    L5_lold
    L3_lold
    L1_lold
	L1_migration_rate
	L1_med_age
	lrgdppc
    L1_lrgdppc
;
#delimit cr

// -----------------------------------------------------------------------------
// 4. Define Treatment Flags and Eligibility (±15 years around takeover)
// -----------------------------------------------------------------------------

// ------------------- Any Populist Treatment Flags ------------------------- //
gen first_year_any = year if first_atakeover
bys cid: egen trperiod_any = min(first_year_any)
gen eligible_any = (trperiod_any - 15 >= 1950)
bys cid: egen trunit_any = max(eligible_any & first_atakeover)
replace trperiod_any = . if trunit_any == 0

// ----------------- Left-Wing Populist Treatment Flags --------------------- //
gen first_year_left = year if first_ltakeover
bys cid (year): replace first_year_left = first_year_left[_n-1] if missing(first_year_left)
bys cid: egen trperiod_left = min(first_year_left)

gen trunit_left = 0
replace trunit_left = 1 if first_ltakeover & trperiod_left >= 1950
replace trperiod_left = . if trunit_left == 0

// ---------------- Right-Wing Populist Treatment Flags --------------------- //
gen first_year_right = year if first_rtakeover
bys cid (year): replace first_year_right = first_year_right[_n-1] if missing(first_year_right)
bys cid: egen trperiod_right = min(first_year_right)

gen trunit_right = 0
replace trunit_right = 1 if first_rtakeover & trperiod_right >= 1950
replace trperiod_right = . if trunit_right == 0

// -----------------------------------------------------------------------------
// 5. Run Synthetic Control (stacked SCM for "any" populist cases)
// -----------------------------------------------------------------------------

// Drop countries with lrgdppc = .
#delimit ;
drop if inlist(country,
				"Croatia", "Czech Republic", "Estonia", "Latvia", "Lithuania",
				"Slovakia", "Slovenia", "Taiwan", "Venezuela") ;

allsynth lold `preds',
	pvalues(rmspe variance)
	bcorrect(merge)
    stacked( trunits(trunit_any)
			 trperiods(trperiod_any), clear
			 eventtime(-15 15)
			 figure( classic placebos ci,
					 xlabel(-15(5)15, labsize(medlarge) nogrid)
					 ylabel(, labsize(medlarge) nogrid angle(0))
					 legend(position(6) ring(2))
					 // graphregion(color(white)) 
					 // plotregion(color(white))
					 save(${figures}/scm_old_any, replace)
				   )
           )
    keep(${data_clean}/old_all.dta, replace) ;

allsynth lold `preds',
	pvalues(rmspe variance)
	bcorrect(merge)
    stacked( trunits(trunit_left)
			 trperiods(trperiod_left), clear
			 eventtime(-15 15)
			 figure( classic placebos ci,
					 xlabel(-15(5)15, labsize(medlarge) nogrid)
					 ylabel(, labsize(medlarge) nogrid angle(0))
					 legend(position(6) ring(2))
					 // graphregion(color(white)) 
					 // plotregion(color(white))
					 save(${figures}/scm_old_left, replace)
				   )
           )
    keep(${data_clean}/old_left.dta, replace) ;

allsynth lold `preds',
	pvalues(rmspe variance)
	bcorrect(merge)
    stacked( trunits(trunit_right)
			 trperiods(trperiod_right), clear
			 eventtime(-15 15)
			 figure( classic bcorrect placebos ci,
					 xlabel(-15(5)15, labsize(medlarge) nogrid)
					 ylabel(, labsize(medlarge) nogrid angle(0))
					 legend(position(6) ring(2))
					 // graphregion(color(white)) 
					 // plotregion(color(white))
					 save(${figures}/scm_old_right, replace)
				   )
           )
    keep(${data_clean}/old_right.dta, replace) ;
#delimit cr
