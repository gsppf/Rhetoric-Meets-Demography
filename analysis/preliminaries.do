// ============================================================================
//  AUTHOR:        GIUSEPPE FAÈ
//  CREATED:       APRIL 26, 2025
//  MODIFIED:      MAY 27, 2025
//  DESCRIPTION:   PRELIMINARY EVIDENCE: POPULIST LEADERS & DEMOGRAPHICS
// ============================================================================

clear
graph set window fontface "Times New Roman"

********************************************************************************
// DESCRIPTIVE STATISTICS
********************************************************************************

use ${data_clean}/populist_demographics, clear
keep if year >= 1950

// Count populist years
// Count total number of observations
quietly count
local N = r(N)

// Count the number of years in which pop==1, lpop==1, and rpop==1
quietly count if pop==1
local Npop = r(N)

quietly count if lpop==1
local Nleft = r(N)

quietly count if rpop==1
local Nright = r(N)

// Display numbers of treated years
display as text "→ Total obs in panel:" as result `N'
display as text "→ Number of populist years:" as result `Npop'
display as text "→ Number of left-wing populist years:" as result `Nleft'
display as text "→ Number of right-wing populist years:" as result `Nright'

// Display shares of treated years
display as text "→ Share of populist years:" as result %6.2f (`Npop'/`N')*100 "%"
display as text "→ Share of left-wing populist years:" as result %6.2f (`Nleft'/`N')*100 "%"
display as text "→ Share of right-wing populist years:" as result %6.2f (`Nright'/`N')*100 "%"

// Shades of populism
display as text "→ Left‐wing share:" as result %6.2f (`Nleft'/`Npop')*100 
display as text "→ Right‐wing share:" as result %6.2f (`Nright'/`Npop')*100 "%"
 
clear

********************************************************************************
// FIGURES 1 & 2: POPULIST LEADERS THROUGH TIME AND POLITICAL SPECTRUM
********************************************************************************

use ${data_clean}/populist_demographics, clear

// Duplicate last year 3x for visualization
bysort cid (year): gen order = _n
by cid: gen last = _n == _N
expand 3 if last
bysort cid year (order): gen copy = _n
replace year = year + copy - 1 if copy != .
drop order copy last

// Clean duplicates & adjust for 2019 panel
duplicates report cid year
bysort cid year (iso): gen expand_group = _n if year == 2019
bysort cid (year expand_group): replace year = year + expand_group - 1 if expand_group < .

// Final year adjustments
replace pop = . if year == 2021
replace pop = 0 if year == 2020 & (iso == "GRC" | iso == "BOL")
replace lpop = 0 if year == 2020 & (iso == "GRC" | iso == "BOL")

// Aggregate counts by year
foreach v in independent pop lpop rpop {
    egen `v'by = sum(`v'), by(year)
}

// Share calculations
gen sh  = popby / independentby * 100
gen shl = lpopby / independentby * 100
gen shr = rpopby / independentby * 100
gen zero = 0

// Classification and labels
gen lrstr = "Left-wing populism" if lpop == 1
replace lrstr = "Right-wing populism" if rpop == 1
egen pop_id = max(pop == 1), by(country)
replace pop = 1 if year == 2021 & pop_id == 1
replace lrstr = "2021" if year == 2021 & pop_id == 1

// Label 2021 populist leaders by country
gen populists = ""

replace populists="Yrigoyen, the Peróns, Menem, the Kirchners"   if year==2021 & country=="Argentina"
replace populists="Estenssoro/Zuazo (MNR), Morales"              if year==2021 & country=="Bolivia"
replace populists="Vargas, Collor, Bolsonaro"                    if year==2021 & country=="Brazil"
replace populists="Borisov"                                      if year==2021 & country=="Bulgaria"
replace populists="Alessandri/Ibáñez"                            if year==2021 & country=="Chile"
replace populists="Ibarra, Bucaram, Correa"                      if year==2021 & country=="Ecuador"
replace populists="Hitler"                                       if year==2021 & country=="Germany"
replace populists="Tsipras"                                      if year==2021 & country=="Greece"
replace populists="Orbán"                                        if year==2021 & country=="Hungary"
replace populists="I. Gandhi, Modi"                              if year==2021 & country=="India"
replace populists="Sukarno, Widodo"                              if year==2021 & country=="Indonesia"
replace populists="Netanyahu"                                    if year==2021 & country=="Israel"
replace populists="Mussolini, Berlusconi, Lega/M5S"              if year==2021 & country=="Italy"
replace populists="Koizumi"                                      if year==2021 & country=="Japan"
replace populists="Cárdenas, Echeverría, Obrador"                if year==2021 & country=="Mexico"
replace populists="Muldoon"                                      if year==2021 & country=="New Zealand"
replace populists="Zuma"                                         if year==2021 & country=="South Africa"
replace populists="Roh"                                          if year==2021 & country=="South Korea"
replace populists="Chen"                                         if year==2021 & country=="Taiwan"
replace populists="T. Shinawatra"                                if year==2021 & country=="Thailand"
replace populists="Erdogan"                                      if year==2021 & country=="Turkey"
replace populists="Johnson"                                      if year==2021 & country=="United Kingdom"
replace populists="Trump"                                        if year==2021 & country=="United States"
replace populists="Mečiar, Fico"                                 if year==2021 & country=="Slovakia"
replace populists="Kaczyńskis, PiS"                              if year==2021 & country=="Poland"
replace populists="García, Fujimori"                             if year==2021 & country=="Peru"
replace populists="Estrada, Duterte"                             if year==2021 & country=="Philippines"
replace populists="Chávez/Maduro"                                if year==2021 & country=="Venezuela"

// Indicators for plotting
gen post1950 = year >= 1950
gen x1950 = 1950 if year >= 1900 & year < 2020

// --- FIGURE 1: Share of Populist Governments ---
twoway ///
    (rarea shl sh year if year <= 1950, ///
        color(gs5*0.5) lcolor(gs13) lpattern(solid) lwidth(vvthin) msize(vsmall) sort) ///
    (rarea shl zero year if year <= 1950, ///
        color(gs11*0.5) lcolor(gs15) lpattern(solid) lwidth(vvthin) msize(vsmall) sort) ///
    (rarea shl sh year if year >= 1950, ///
        color(gs5) lcolor(gs6) lpattern(solid) lwidth(vvthin) msize(vsmall) sort) ///
    (rarea shl zero year if year >= 1950, ///
        color(gs11) lcolor(gs11) lpattern(solid) lwidth(vvthin) msize(vsmall) sort) ///
    (line sh year if year <= 1950, ///
        lcolor(red) lpattern(dash) lwidth(thin) msize(vsmall) sort) ///
    (line sh year if year >= 1950, ///
        lcolor(red) lpattern(solid) lwidth(thick) msize(vsmall) sort) ///
    if year >= 1900 & year < 2020, ///
    xlabel(1900 1920 1940 1950 1960 1980 2000 2019, labsize(medsmall)) ///
	xscale(range(1900 2019)) ///
    ylabel(0(5)25, labsize(medsmall) angle(0) nogrid) ///
    xtitle("", size(medsmall) margin(medium)) ///
    ytitle("Share of independent countries with populist government (%)", ///
           size(small) margin(medium)) ///
    scheme(s1mono) ///
    graphregion(color(white) lcolor(white)) ///
	plotregion(margin(0)) ///
    ysize(6) xsize(9) ///
    legend(rows(1) order(6 3 4) ///
           label(6 "Populist governments") ///
           label(3 "Right-wing populism") ///
           label(4 "Left-wing populism") ///
           symxsize(*0.3) symysize(*0.3) ///
           region(lwidth(none)) ///
           size(medlarge)) ///
    title("Populists in Power: Share of Countries in Sample", ///
          size(large) margin(medium)) ///
    xline(1950, lcolor(black) lpattern(dash) lwidth(medium))
  
/*
   Note: "Share of populist governments in all governments in sample of (up to) 
   60 independent countries, 1900–2020. We consider any country-year in which a 
   populist was the effective ruler (i.e., president, prime minister, 
   or equivalent)."
*/

graph export ${figures}/Figure1.pdf, replace

// --- FIGURE 2: Populist Leader Spells by Country ---
egen treated_order = group(country) if pop_id == 1, label
label values treated_order treated_order
gen ypos = treated_order

twoway ///
    (scatter ypos year if year < 1950 & lpop == 1, msymbol(square) mcolor(cranberry*0.5) msize(1.675) yaxis(1)) ///
    (scatter ypos year if year < 1950 & rpop == 1, msymbol(square) mcolor(navy*0.5) msize(1.675) yaxis(1)) ///
    (scatter ypos year if year >= 1950 & lpop == 1, msymbol(square) mcolor(cranberry) msize(1.675) yaxis(1)) ///
    (scatter ypos year if year >= 1950 & rpop == 1, msymbol(square) mcolor(navy) msize(1.675) yaxis(1)), ///
	xline(1949.11, lcolor(black) lpattern(dash) lwidth(medium)) ///
    ylabel( ///
        1 "Argentina" 2 "Bolivia" 3 "Brazil" 4 "Bulgaria" 5 "Chile" 6 "Ecuador" 7 "Germany" ///
        8 "Greece" 9 "Hungary" 10 "India" 11 "Indonesia" 12 "Israel" 13 "Italy" 14 "Japan" ///
        15 "Mexico" 16 "New Zealand" 17 "Peru" 18 "Philippines" 19 "Poland" 20 "Slovakia" ///
        21 "South Africa" 22 "South Korea" 23 "Taiwan" 24 "Thailand" 25 "Turkey" 26 "United Kingdom" ///
        27 "United States" 28 "Venezuela", axis(1) labsize(small) labgap(medium) angle(0) grid) ///
	yscale(reverse) ///
    xlabel(1900(20)2020 1949.11 `"1950"', labsize(small)) ///
    xtitle("") xscale(titlegap(2)) ///
    ytitle("") ///
    legend(order(3 "Left-wing populism" 4 "Right-wing populism") ///
           region(color(white)) rows(1) size(medium)) ///
    scheme(s2mono) graphregion(color(white) margin(0 10 0 0)) ///
	title("Populists Leader Spells by Country", size(medlarge) margin(medium)) ///
	ysize(17) xsize(20)
	
/* 
   Note: "The figure includes those 28 countries of our 60-country sample that 
   had a populist in power at least once since 1900 or independence, that is, 
   the countries that are also featured in Table 1. The gray bars refer to the 
   populist spells given in panel A of Table 1.", size(small) span
*/
	
graph export ${figures}/Figure2.pdf, replace

clear

********************************************************************************
// TABLE 2: REGRESSIONS ON FERTILITY, MIGRATION, OLD-AGE
********************************************************************************

use ${data_clean}/populist_demographics, replace

tsset cid year

// Log-transform variables of interest
gen lfert  = log(fertility)
gen lold   = log(oldage_ratio)

gen dlfert = 100 * D.lfert      // % change in fertility
gen dlold  = 100 * D.lold       // % change in old-age ratio
gen dmig   = D.migration_rate   // change in net migrants per 1 000

estimates clear
// for fertility and old-age dependency, use dlfert and dlold
local grow_outcomes dlfert dlold

foreach v of local grow_outcomes {
    foreach post in Post_3 Post_5 Post_15 {
        // simple
        // simple
        eststo si_`v'_`post':   quietly reg `v'    `post', robust
        // fixed effects
        eststo fe_`v'_`post':   quietly reg `v' i.year i.cid `post', robust
        // macro + controls
        eststo ma_`v'_`post':   quietly reg `v' i.year i.cid ///
            L1.institutions L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis ///
            L1.tradegdp     L1.inflation    `post', robust
    }
}

// for migration, switch from levels to dmig
foreach post in Post_3 Post_5 Post_15 {
    eststo si_mig_`post':    quietly reg dmig  `post', robust
    eststo fe_mig_`post':    quietly reg dmig  i.year i.cid `post', robust
    eststo ma_mig_`post':    quietly reg dmig  i.year i.cid ///
            L1.institutions L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis ///
            L1.tradegdp     L1.inflation    `post', robust
}

// 3) Export
esttab ///
    si_dlfert_Post_3  fe_dlfert_Post_3  ma_dlfert_Post_3  ///
    si_dlfert_Post_5  fe_dlfert_Post_5  ma_dlfert_Post_5  ///
    si_dlfert_Post_15 fe_dlfert_Post_15 ma_dlfert_Post_15 ///
    using ${tables}/Table2_fert.tex,  replace ///
    keep(Post_3 Post_5 Post_15) se r2 b(2) se(2) obslast ///
    starlevels(* 0.1 ** 0.05 *** 0.01) ///
    title("Effect of Populism on Fertility Growth (%)") ///
    nonotes label eqlabels(none) mlabels(none)

esttab ///
    si_mig_Post_3  fe_mig_Post_3  ma_mig_Post_3  ///
    si_mig_Post_5  fe_mig_Post_5  ma_mig_Post_5  ///
    si_mig_Post_15 fe_mig_Post_15 ma_mig_Post_15 ///
    using ${tables}/Table2_mig.tex, replace ///
    keep(Post_3 Post_5 Post_15) se r2 b(2) se(2) obslast ///
    starlevels(* 0.1 ** 0.05 *** 0.01) ///
    title("Effect of Populism on Δ Net Migration (per 1 000)") ///
    nonotes label eqlabels(none) mlabels(none)

esttab ///
    si_dlold_Post_3  fe_dlold_Post_3  ma_dlold_Post_3  ///
    si_dlold_Post_5  fe_dlold_Post_5  ma_dlold_Post_5  ///
    si_dlold_Post_15 fe_dlold_Post_15 ma_dlold_Post_15 ///
    using ${tables}/Table2_old.tex, replace ///
    keep(Post_3 Post_5 Post_15) se r2 b(2) se(2) obslast ///
    starlevels(* 0.1 ** 0.05 *** 0.01) ///
    title("Effect of Populism on Old-Age Dependency Growth (%)") ///
    nonotes label eqlabels(none) mlabels(none)

	   capture drop _est_*
	   
drop dlfert dlold
clear

// Disentangle growth‐rate effects for 65+ vs. 15–64
use ${data_clean}/populist_demographics.dta, clear
keep if year >= 1950
tsset cid year

// 1) Construct age‐group series and their annual % changes
// ---------------------------------------------------------
gen pop65plus = age65_69 + age70_74 + age75_79 + age80_84 + ///
                age85_89 + age90_94 + age95_99 + age100
gen pop1564   = age15_19 + age20_24 + age25_29 + age30_34 + ///
                age35_39 + age40_44 + age45_49 + age50_54 + ///
                age55_59 + age60_64

// year‐on‐year percent change:
gen l65 = log(pop65plus) 
gen l15 = log(pop1564)
gen dg65 = 100 * D.l65
label var dg65 "Δ log Pop\,65+ (%)"
gen dg15 = 100 * D.l15
label var dg15 "Δ log Pop\,15–64 (%)"

// 2) Three‐horizon regressions on these %­changes
// -----------------------------------------------
estimates clear
local horizons Post_3 Post_5 Post_15

foreach outcome in dg65 dg15 {
    foreach post of local horizons {
        // simple OLS
        eststo si_`outcome'_`post': quietly reg `outcome' `post', robust

        // country+year fixed effects
        eststo fe_`outcome'_`post': quietly reg `outcome' i.cid i.year `post', robust

        // + macro & institution controls
        eststo ma_`outcome'_`post': quietly reg `outcome' i.cid i.year ///
            L1.institutions L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis ///
            L1.tradegdp     L1.inflation    `post', robust
    }
}

// 3) Export tables
// ----------------
esttab ///
    si_dg65_Post_3  fe_dg65_Post_3  ma_dg65_Post_3  ///
    si_dg65_Post_5  fe_dg65_Post_5  ma_dg65_Post_5  ///
    si_dg65_Post_15 fe_dg65_Post_15 ma_dg65_Post_15 ///
    using ${tables}/TableA1_65.tex, replace ///
    keep(Post_3 Post_5 Post_15) se r2 b(2) se(2) obslast ///
    starlevels(* 0.1 ** 0.05 *** 0.01) ///
    title("Effect of Populism on Δ Log Pop\,65+ (%)") ///
    nonotes label eqlabels(none) mlabels(none)

esttab ///
    si_dg15_Post_3  fe_dg15_Post_3  ma_dg15_Post_3  ///
    si_dg15_Post_5  fe_dg15_Post_5  ma_dg15_Post_5  ///
    si_dg15_Post_15 fe_dg15_Post_15 ma_dg15_Post_15 ///
    using ${tables}/TableA2_1564.tex, replace ///
    keep(Post_3 Post_5 Post_15) se r2 b(2) se(2) obslast ///
    starlevels(* 0.1 ** 0.05 *** 0.01) ///
    title("Effect of Populism on Δ Log Pop\,15–64 (%)") ///
    nonotes label eqlabels(none) mlabels(none)

drop pop65plus pop1564 dg65 dg15
clear

********************************************************************************
* FIGURE 3: DEMOGRAPHIC PERFORMANCE GAPS — LEVELS
********************************************************************************

use ${data_clean}/populist_demographics.dta, clear
tsset cid year

* 0) Prepare treatment timing *
bys cid (year): gen cum_take = sum(atakeover)
gen first_take = year if atakeover==1 & cum_take==1
bys cid: egen treat_year = min(first_take)
drop first_take cum_take

* 1) Create level outcomes *
gen lfert = log(fertility)
gen lold  = log(oldage_ratio)
gen mig   = migration_rate

* 2) Extract (cid × treat_year) into tiny file *
preserve
  keep if atakeover==1
  keep cid year
  rename year treat_year
  tempfile eps
  save `eps'
restore

* 3) For each horizon h={3,5,15}, compute cross-country average gaps in levels *
tempfile agg3 agg5 agg15
foreach h in 3 5 15 {
  preserve
    merge m:1 cid treat_year using `eps', keep(master match) nogenerate
    keep if year == treat_year + `h'

    * — country gap: difference from unit-specific mean at h —
    bysort cid:  egen m_lfert = mean(lfert)
    bysort cid:  egen m_lold  = mean(lold)
    bysort cid:  egen m_mig   = mean(mig)
    gen  gap_c_fert = lfert - m_lfert
    gen  gap_c_old  = lold  - m_lold
    gen  gap_c_migr = mig   - m_mig

    * — global gap: difference from cross-country mean at year —
    bysort year: egen g_lfert = mean(lfert)
    bysort year: egen g_lold  = mean(lold)
    bysort year: egen g_mig   = mean(mig)
    gen  gap_g_fert = lfert - g_lfert
    gen  gap_g_old  = lold  - g_lold
    gen  gap_g_migr = mig   - g_mig

    collapse (mean) gap_c_fert gap_g_fert ///
                    gap_c_old  gap_g_old  ///
                    gap_c_migr gap_g_migr
    gen horizon = `h'

    save agg`h', replace
  restore
}

* 4) Stack horizons & reshape for plotting *
use agg3, clear
append using agg5
append using agg15
reshape long gap_c_ gap_g_, i(horizon) j(metric) string

* 5) dodge positions so bars don't overlap *
gen x_c = horizon - 0.2
gen x_g = horizon + 0.2

* 6) Loop over each metric & twoway‐bar *
levelsof metric, local(ms)
foreach m of local ms {
    preserve
        keep if metric=="`m'"

        local lab = cond("`m'"=="fert","Fertility", ///
                    cond("`m'"=="old", "Old-Age Dependency", "Migration"))

        twoway ///
          (bar gap_c x_c, barwidth(0.6) fcolor(white) lcolor(black)) || ///
          (bar gap_g x_g, barwidth(0.6) fcolor(gs12)  lcolor(black)),     ///
          xlabel(3 5 15, labsize(medlarge))                                ///
          ylabel(, angle(0) nogrid)                                       ///
          yline(0, lcolor(black) lwidth(medium))                          ///
          yscale()                                                        ///
          legend(position(6) ring(2) rows(1) region(lcolor(white)))       ///
          title("Performance gap in `lab'", size(large) margin(medium))   ///
          graphregion(color(white))

        graph export "${figures}/Figure3_`m'_levels.pdf", replace
    restore
}

* 7) (Optional) report the numeric gaps instead of graphs *
capture program drop report_gaps
program define report_gaps
    syntax , HORIZONS(numlist) VAR(string)
    foreach h of local horizons {
        preserve
            merge m:1 cid treat_year using `eps', keep(master match)
            keep if year == treat_year + `h'
            by cid: egen m_x = mean(`var')
            gen gap_c = `var' - m_x
            by year: egen g_x = mean(`var')
            gen gap_g = `var' - g_x

            quietly summarize gap_c
            di as result %6.2f r(mean) ///
               " = country gap in `var' at `h' yrs"
            quietly summarize gap_g
            di as result %6.2f r(mean) ///
               " =   global  gap in `var' at `h' yrs" _newline
        restore
    }
end
