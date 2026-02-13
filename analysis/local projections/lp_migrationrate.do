// AUTHOR:            GIUSEPPE FAÈ  
// CREATED:           JUNE 14, 2025
// DESCRIPTION:       LP-IV for migration rate analysis following Funke et al. methodology

use ${data_clean}/populist_demographics.dta, clear

keep if year >= 1950

xtset cid year

// Forward horizon "changes" (in the same spirit as old_gr*)
gen dmig = migration_rate - L1.migration_rate
label var dmig "Δ migration rate (per 1 000)"

forvalues h = 1/15 {
    gen mig_ch`h' = F`h'.migration_rate - migration_rate
    label var mig_ch`h' "Δ migration rate Y+`h' (per 1 000)"
}

/* Interpretation: As migration rate is "net migrants per 1,000 population," a 
   change of x simply means x more (net) migrants per 1 000.
   	•	If x > 0, you have x additional net immigrants per 1 000 people.
	•	If x < 0, you have |x| additional net emigrants per 1 000 people.
   
   EXAMPLE: x = 2 at horizon 1, you interpret that as "an increase of 2 net 
   migrants per 1 000 population in the year after treatment." In a hypothetical 
   country of 1 million people that would correspond to about 2 000 extra net 
   migrants, but the rate itself is always per 1 000.
   
   Net migration changed by x (increase for x > 0 , decrease for x < 0) for 
   every 1000 people. 
*/


// 2. Create baseline fertility growth and cross-year mean
egen m_dmig = mean(dmig), by(year)
label var m_dmig "World mean Δ migration rate (per 1 000)"


// 3. Create demographic control variables
#delimit ;   
	gen double work = age15_19 + age20_24 + age25_29 + age30_34 +
					  age35_39 + age40_44 + age45_49 + age50_54 +
					  age55_59 + age60_64 ;
#delimit cr


// 4. Create GDP controls (following original methodology)
gen lrgdppc = log(fstgdp)
gen grlrgdppc = (lrgdppc - L1.lrgdppc) * 100
egen mgrlrgdppc = mean(grlrgdppc), by(year)

// 5. Treatment indicators (following original naming)
rename placebo   np
rename atakeover ap  
rename ltakeover le
rename rtakeover ri

// 6. Zero-out placebo during ±15 windows around populist takeovers
forvalues h = 1/15 {
	replace np = 0 if F`h'.ap == 1
	replace np = 0 if L`h'.ap == 1
}

// 7. Create time axis & placeholders
gen years = _n-1 if _n<=16
gen zero  = 0
foreach t in np ap le ri {
    foreach v in irf se up lo {
        gen `v'_`t' = 0
    }
}  

// 3. log mid‐year population  
gen lpop_jul       = log(pop_jul)       if pop_jul       > 0  

// 8. Define estimation sample via h=15 LP
#delimit ;
xtreg mig_ch15 
		np ap le ri 
		L(1/5).work
		L(1/5).bankcrisis
		L(1/5).institutions
		L(1/5).unemployrate
		// L(1/5).pop_jul
		L(1/5).advanced
		L(1/5).migration_rate
		L(1/5).dmig
		L(1/5).m_dmig
		, fe;
#delimit cr

gen regsample = 1 if e(sample)

// 9. Horizon loop: extract IRF_np & IRF_ap
forvalues h = 1/15 {
    #delimit ;
    xtreg mig_ch`h' 
		np ap 
		L(1/5).work
		L(1/5).bankcrisis
		L(1/5).institutions
		L(1/5).unemployrate
		// L(1/5).pop_jul
		L(1/5).advanced
		L(1/5).migration_rate
		L(1/5).dmig
		L(1/5).m_dmig
		if regsample==1, fe;
    #delimit cr

    // test difference
    test np = ap
    estadd scalar NonpPop_pdiff = r(p)

    // IRF_np
    lincom _cons + np
    replace irf_np = r(estimate) if years==`h'
    replace se_np  = r(se)       if years==`h'
    estadd scalar R2           = e(r2)
    estadd scalar Observations = e(N)

    // IRF_ap
    lincom _cons + ap
    replace irf_ap = r(estimate) if years==`h'
    replace se_ap  = r(se)       if years==`h'
}

// Sanity ckeck
gen double irf_diff = irf_ap - irf_np
list years irf_np irf_ap irf_diff if years <= 15, clean noobs

// 10. Compute 95% bands & gap
replace up_np  = irf_np + 1.96 * se_np if years<=15
replace lo_np  = irf_np - 1.96 * se_np if years<=15
gen irf_ap_gap = irf_ap - irf_np       if years<=15

// 11. Panel A: Projected trends
twoway (rarea up_np lo_np years, fcolor(gs12) lcolor(white) lpattern(solid)) ///
       (line irf_np years, lcolor(blue) lpattern(solid) lwidth(thick)) ///
       (line irf_ap years, lcolor(red) lpattern(shortdash) lwidth(vthick)), ///
		xlabel(0(5)15, nogrid labsize(medlarge) angle(0)) ///
		ylabel(, nogrid labsize(medlarge) angle(0)) ///
		legend(rows(1) position(6) ring(2) ///
			   label(3 "All populists") ///
			   label(2 "Trend in other years") ///
			   label(1 "95% CI") ///
			   order(3 2 1) symxsize(*0.375) symysize(*0.375)) ///
		title("Panel A: Net Migration Projected Trends", size(large) margin(medium)) ///
		ytitle("Δ net migration rate (per 1,000 pop)", size(medsmall) margin(small)) ///
		xtitle("", size(medsmall)) ///
		graphregion(color(white)) plotregion(color(white)) ///
		name(pl, replace) nodraw

// 12. Panel B: left vs right gaps
gen irf_le_gap = .
gen irf_ri_gap = .

foreach t in le ri {
    forvalues h = 1/15 {
    #delimit ;
      xtreg mig_ch`h'  np `t'
	    L(1/5).work
		L(1/5).institutions
		L(1/5).unemployrate
		// L(1/5).pop_jul
		L(1/5).advanced
		L(1/5).migration_rate
		L(1/5).dmig
		L(1/5).m_dmig
        if regsample==1, fe;
     #delimit cr

        lincom _cons + `t'
        replace irf_`t'     = r(estimate) if years==`h'
        replace se_`t'      = r(se)       if years==`h'
    }
    replace irf_`t'_gap = irf_`t' - irf_np if years<=15
}

// 13. Plot Panel B
twoway (line irf_ap_gap years, lcolor(black) lpattern(solid) lwidth(thick)) ///
	   (line irf_le_gap years, lcolor(cranberry) lpattern(dash) lwidth(thick)) ///  
	   (line irf_ri_gap years, lcolor(navy) lpattern(dash_dot) lwidth(thick)) ///
	   (line zero years, lcolor(gs8) lpattern(longdash)) if years <=15, ///
	   xlabel(, nogrid labsize(medlarge) angle(0)) ///
		ylabel(, nogrid labsize(medlarge) angle(0)) ///
       legend(rows(1) position(6) ring(2) label(3 "Right populist") label(2 "Left populist") ///
	   label(1 "All populist") order(1 2 3) symxsize(*0.375) ///
	   symysize(*0.375) size(medlarge) region(lwidth(none))) ///
	   subtitle("", size(small)) xlabel(, labsize(large)) ///
	   ylabel(, labsize(medlarge) angle(0)) ///
	   title("Panel B: Net Migration Projected Gaps", color(black) size(large) margin(medium)) ///
	   xtitle("") ytitle("Δ net migration rate (per 1,000 pop)") graphregion(color(white)) name(pr, replace) nodraw
	   

// 14. Combine & export
gr combine pl pr, rows(1) iscale(0.75) ///
				  graphregion(color(white) margin(0 0 0 0)) ///
				  imargin(5 5) xsize(10) ysize(5) ///
				  title("Net Migration Paths after Populist Governments Enter into Office: Local Projections" , ///
          size(medlarge) margin(medium))
		  
graph export ${figures}/Figure4_migration.pdf, replace

// 15. Table C4
esttab LP* using ${tables}/TableC4_migration.tex, replace ///
    label stats(irf_np se_np irf_ap se_ap R2 NonpPop_pdiff Observations) ///
    starlevels(* 0.1 ** 0.05 *** 0.01) nonotes

clear
