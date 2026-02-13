// AUTHOR:            GIUSEPPE FAÈ  
// CREATED:           JUNE 14, 2025
// DESCRIPTION:       Paths for fertility after Populist Governmets Enter into 
//					  Office: Local Projection

use ${data_clean}/populist_demographics.dta, clear

keep if year >= 1950

xtset cid year

// 1. Create log fertility and forward growth rates (dependent variables)
gen lfert  = log(fertility)
gen dlfert = 100 * D.lfert  // 1-year growth in log-fertility

forvalues h = 1/15 {
    gen dlfert_fut`h' = F`h'.dlfert
    label var dlfert_fut`h' "Δ log‐fertility, t+`h'"
}

// 2. Create baseline fertility growth and cross-year mean
egen mdlfert  = mean(dlfert), by(year)

// 3. Create demographic control variables
#delimit ;   
	gen double share_young   = (age0_4 + age5_9 + age10_14) / pop_jul ;
	gen double share_working = (age15_19 + age20_24 + age25_29 + age30_34 +
								age35_39 + age40_44 + age45_49 + age50_54 +
								age55_59 + age60_64) / pop_jul ;
	gen double share_elderly = (age65_69 + age70_74 + age75_79 + age80_84 +
								age85_89 + age90_94 + age95_99 + age100) 
								/ pop_jul ;
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

// 8. Define estimation sample via h=15 LP
#delimit ;
xtreg dlfert_fut15 
		np ap le ri 
		L(1/5).female
		L(1/5).pop_jul 
		L(1/5).lrgdppc
		L(1/5).grlrgdppc
		// L(1/5).bankcrisis
		L(1/5).mgrlrgdppc
		L(1/5).institutions
		L(1/5).migration
		L(1/5).lfert 
		L(1/5).mdlfert 
		L(1/5).dlfert
		, fe;
#delimit cr

gen regsample = 1 if e(sample)

// 9. Horizon loop: extract IRF_np & IRF_ap
forvalues h = 1/15 {
    #delimit ;
    xtreg dlfert_fut`h' 
			np ap
			L(1/5).female
			L(1/5).pop_jul 
			L(1/5).lrgdppc
			L(1/5).grlrgdppc
			// L(1/5).bankcrisis
			L(1/5).mgrlrgdppc 
			L(1/5).institution
			L(1/5).migration
			L(1/5).lfert 
			L(1/5).mdlfert 
			L(1/5).dlfert
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
	eststo LP`h'
}

// 10. Compute 95% bands & gap
replace up_np  = irf_np + 1.96 * se_np if years<=15
replace lo_np  = irf_np - 1.96 * se_np if years<=15
gen irf_ap_gap = irf_ap - irf_np       if years<=15

// 11. Panel A: Projected trends
twoway (rarea up_np lo_np years, fcolor(gs12) lcolor(white) lpattern(solid)) ///
       (line irf_np years, lcolor(blue) lpattern(solid) lwidth(thick)) ///
       (line irf_ap years, lcolor(red) lpattern(shortdash) lwidth(vthick)), ///
		xlabel(0(5)15, nogrid) ///
		ylabel(/*-2(2)8*/, nogrid labsize(medlarge) angle(0)) ///
		yscale(range(/*-2 8*/)) ///
		legend(rows(1) position(6) ring(2) ///
			   label(3 "All populists") ///
			   label(2 "Trend in other years") ///
			   label(1 "95% CI") ///
			   order(3 2 1)) ///
		title("Panel A: Fertility Projected Trends", size(large)) ///
		ytitle("Percent (100 × log)", size(medsmall) margin(small)) ///
		xtitle("", size(medsmall)) ///
		xlabel(, nogrid) ///
		graphregion(color(white)) plotregion(color(white)) ///
		name(pl, replace) nodraw

// 12. Panel B: left vs right gaps
gen irf_le_gap = .
gen irf_ri_gap = .

foreach t in le ri {
    forvalues h = 1/15 {
        // regress cumulative Δ log‐fertility at horizon h
      #delimit ;  
		xtreg dlfert_fut`h'
            np `t'
            L(1/5).female
            L(1/5).pop_jul
            L(1/5).lrgdppc
			// L(1/5).grlrgdppc
			// L(1/5).bankcrisis
            L(1/5).mgrlrgdppc
            L(1/5).institutions 
            L(1/5).migration
            L(1/5).lfert
            L(1/5).mdlfert
            L(1/5).dlfert
            if regsample==1, fe ;
	  #delimit cr
        // pull out the IRF for this type
        lincom _cons + `t'
        replace irf_`t' = r(estimate) if years==`h'
        replace se_`t'  = r(se)       if years==`h'
    }
    // compute the gap vs. non‐populists
    replace irf_`t'_gap = irf_`t' - irf_np if years <= 15
}

// 13. Plot Panel B
twoway (line irf_ap_gap years, lcolor(black) lpattern(solid) lwidth(thick)) ///
	   (line irf_le_gap years, lcolor(cranberry) lpattern(dash) lwidth(thick)) ///  
	   (line irf_ri_gap years, lcolor(navy) lpattern(dash_dot) lwidth(thick)) ///
	   (line zero years, lcolor(gs8) lpattern(longdash)) if years <=15, ///
	   xlabel(, nogrid) ///
	   ylabel(, nogrid labsize(medlarge) angle(0)) ///
       legend(rows(1) position(6) ring(2) label(3 "Right populist") label(2 "Left populist") ///
	   label(1 "All populist") order(1 2 3) symxsize(*0.375) ///
	   symysize(*0.375) size(medlarge) region(lwidth(none))) ///
	   subtitle("", size(small)) xlabel(, labsize(large)) ///
	   ylabel(, labsize(medlarge) angle(0)) ///
	   title("Panel B: Fertility Projected Gaps", color(black) size(large) margin(medium)) ///
	   xtitle("") ytitle("Percentage Points") graphregion(color(white)) name(pr, replace) nodraw

// 14. Combine & export
gr combine pl pr, rows(1) iscale(0.75) ///
    graphregion(color(white) margin(l=1 r=3 t=1)) ///
    imargin(5 5) xsize(10) ysize(5) ///
	title("Fertility Paths after Populist Governments Enter into Office: Local Projections" , ///
          size(medlarge) margin(medium))
		  
graph export ${figures}/Figure4_fertility_msk.pdf, replace

// 15. Table C4
esttab LP* using ${tables}/TableC4_fertility.tex, replace ///
    label stats(irf_np se_np irf_ap se_ap R2 NonpPop_pdiff Observations) ///
    starlevels(* 0.1 ** 0.05 *** 0.01) nonotes
	
clear
