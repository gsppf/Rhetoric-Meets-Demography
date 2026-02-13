//	AUTHOR:			GIUSEPPE FAÈ
//	CREATED:		JUNE 15, 2025
//	MODIFIED: 		
//	DESCRIPTION:	This file generaets IPWRA-LP for Old-Age-Dependency Ratio 

use ${data_clean}/populist_demographics.dta, clear

keep if year >= 1950

xtset cid year

egen ccode = group(iso)
tsset ccode year, yearly

gen lold    = log(oldage_ratio)		                              
gen dlold   = 100 * d.lold

#delimit ;
gen old = age65_69 +
		  age70_74 +
		  age75_79 +
		  age80_84 +
		  age85_89 +
		  age90_94 +
		  age95_99;
#delimit cr

forvalues i = 1/15 {
	gen lold`i' = 100 * (f`i'.lold - lold)	
	label var lold`i' "Logged Old-Age-Dependency Ratio `i'"
}

gen lrgdppc = log(fstgdp)
gen grlrgdppc = (lrgdppc - L1.lrgdppc) * 100
egen mgrlrgdppc = mean(grlrgdppc), by(year)
egen wdlold  = mean(dlold), by(year)
gen core     = 1 if (year >= 1950 & year<=2003)
replace core = . if placebo == 0 & atakeover == 0 

gen dinstitutions    = d.institutions
replace institutions = dinstitutions

foreach r in bankcrisis currcrisis debtcrisis dlold wdlold institutions war gini unemployrate conflicts old migration med_age pop_jul inflation debtgdp lrgdppc mgrlrgdppc {
qui tssmooth ma `r'_wma = `r',  weights(5 4 3 2 <1>) 
replace `r' = l1.`r'_wma
	}
label var placebo "Non-populist"
label var atakeover "Populist"
label var dlold "Growth rate"
label var wdlold "World growth"
label var institutions "Institutional quality"
label var war "World war"
label var debtgdp "Debt/GDP"
label var conflicts "Social conflicts (polarization)"
label var gini "Income inequality (Gini)"
label var global "Financial openness"
label var koftrade "Trade openness"
label var unemployrate "Unemployment"
label var inflation "Inflation"
label var bankcrisis "Banking crisis"
label var currcrisis "Currency crisis"
label var debtcrisis "Sovereign debt crisis"

local type placebo atakeover 
foreach t of local type {
		gen b`t'0=0  
		gen se`t'0=0  
}

qui sum ccode , d
local c = r(max)
forvalues i =1/`c' {
	gen      dum`i'=0
	replace  dum`i'= 1 - 1/`c' if ccode==`i'
	replace  dum`i'=   - 1/`c' if ccode~=`i'
	}
	
local rhsall bankcrisis currcrisis debtcrisis dlold wdlold institutions war gini unemployrate conflicts old migration med_age pop_jul inflation debtgdp lrgdppc mgrlrgdppc

	foreach v of local rhsall {
	qui	bys iso: sum `v' 
		replace `v' = `v' - r(mean)
		}

local base bankcrisis dlold wdlold institutions inflation war migration
local rhs1  `base'
local rhs2  `base' currcrisis debtcrisis lrgdppc mgrlrgdppc
local rhs3  `base' debtgdp lrgdppc
local rhs4  `base' conflicts
local rhs5  `base' gini mgrlrgdppc
local rhs6  `base' old
local rhs7  `base' med_age
local rhs8  `base' currcrisis debtcrisis debtgdp conflicts gini old med_age
local rhs9  `base' unemployrate	lrgdppc	
		
estimates clear
forvalues i = 1/9{
qui logit atakeover `rhs`i'' dum1-dum59 if core==1
qui eststo mar`i': margins, dydx(*) post 
qui logit atakeover `rhs`i'' dum1-dum59 if core==1
qui eststo mco`i': margins, post	
}

forvalues i = 1/9{
qui logit atakeover `rhs`i'' dum1-dum59 if core==1
qui predict phat`i' if e(sample)==1
qui gen _`i'_sample = 1 if e(sample)			
qui roctab atakeover phat`i' if phat`i'~=.
qui local AUC = `r(area)'
qui estadd local AUC "`:di %6.3f `AUC''"
qui local AUC_se = `r(se)'
qui estadd local AUC_se "`:di %6.3f `AUC_se´''"
qui local Observations = `r(N)'
qui estadd local Observations "`:di %4.0fc `Observations''"
qui est sto _`i' 
}

esttab mar* using ${tables}/TableC1_old.tex, drop(dum*) cell(b(fmt(3) star) se(fmt(3) par)) noobs stats() varwidth(32) eqlabels(none) mlabels(none) starlevels(* 0.1 ** 0.05 *** 0.01) label nonotes replace page prehead("\begin{tabular}{l*{9}{c}}") postfoot("")

esttab mco* using ${tables}/TableC1_old.tex, cell(b(fmt(3) star) se(fmt(3) par)) noobs stats() varwidth(32) starlevels(* 0.1 ** 0.05 *** 0.01) nonumber label nonotes eqlabels(none) mlabels(none) append page postfoot("") prehead("")  

esttab _*  using ${tables}/TableC1_old.tex, drop(*) stats(Observations AUC AUC_se r2_p, fmt(%4.3fc)) modelwidth(9) varwidth(32) label nonotes nonumber eqlabels(none) mlabels(none) append page prehead("") postfoot("\end{tabular}")

twoway (kdensity phat1 if atakeover==1, lpattern(dash) color(red) lwidth(medthick)) (kdensity phat1 if atakeover==0, color(blue) lwidth(thick)), ///
text(5.1 .125 "Distribution for control units", placement(e) color(blue) size()) text(1.6 .400 "Distribution for treated units", placement(e) color(red) size()) ///
title("") ylabel(, labsize(medlarge) angle(0)) xlabel(0(1)1, labsize(medlarge)) ytitle("Frequency") xtitle("Estimated probability of treatment") graphregion(color(white)) ///
title("Stage 1: Logit prediction" , color(black) size(large) margin(medium)) plotregion(lpattern(blank)) scheme(s1color) legend(off) nodraw name(stage1, replace) 

preserve
keep if phat1!=. & atakeover==1
keep phat1 year cid
sort phat1
egen medianprob = median(phat1)
gen predic = 1 if phat1 >=medianprob
gen unpred = 1 if phat1 <medianprob
tostring year cid, replace
gen ca = year + "_" + cid
keep ca predic unpred
cap save ${data_clean}/probabilities_old, replace
restore 

preserve
keep if phat1!=. & atakeover==1
keep phat1 country year
gen No = _n
rename (year country) (Treatment Country)
gen Probability = string(phat1, "%04.2f")
drop phat1
order No
*Table C2
texsave * using ${tables}/TableC2_old.tex, title(TABLE C2) replace
restore

drop if phat1==.
egen newid = group(iso)
qui sum newid 
local c = r(max)

forvalues i =1/`c' {
	gen      d`i'=0
	replace  d`i'= 1 - 1/`c' if newid==`i'
	replace  d`i'=   - 1/`c' if newid~=`i'
	}

gen lessone = `c' - 1
qui sum lessone 
local g = r(mean)

reg lold1 atakeover placebo `rhs1' d1-d`g' phat1 if _1_sample==1, noconstant cluster(iso) 	
		
	foreach v of local rhs1 {
		bys iso: sum `v' if e(sample)==1
		gen `v'a = `v' - r(mean)
		}
				
gen invwt = atakeover/phat1 + (1-atakeover )/(1-phat1) if phat1~=. & e(sample)==1
gen asample = 1 if e(sample)==1

	forvalues i=1/15 {
		reg lold`i' placebo atakeover `rhs1'  d1-d`g' [pweight=invwt] if asample==1, noconstant cluster(iso)
				test atakeover = placebo 				 
			    estadd scalar NonpPop_pdiff = r(p)
			    eststo IPW`i'
			    gen betahplacebo_`i'  = _b[placebo]
			 	gen	sehplacebo_`i'  = _se[placebo]
				gen betahatakeover_`i'  = _b[atakeover]
			 	gen sehatakeover_`i' = _se[atakeover]				
	}
	
forvalues i=1/15 {
foreach t of local type {
replace b`t'0 = betah`t'_`i' if _n==`i'+1
replace se`t'0 = seh`t'_`i' if _n==`i'+1
	}	
	}

gen Years = _n-1 if _n <= 16
label var Years "Years"
gen zero = 0 

cap gen upbplacebo0 = bplacebo0 + 1.645*seplacebo0 
cap gen dnbplacebo0 = bplacebo0 - 1.645*seplacebo0  

twoway ///
    (rarea upbplacebo0 dnbplacebo0 Years, ///
             fcolor(gs12) lcolor(white) lpattern(solid)) ///
    (line  bplacebo0 Years, ///
             lcolor(blue) lpattern(solid) lwidth(thick)) ///
    (line  batakeover0 Years, ///
             lcolor(red) lpattern(shortdash) lwidth(vthick)) ///
    (line  zero Years, ///
             lcolor(black)) if Years <= 15, ///
    xlabel(0(5)15, labsize(medlarge)) ///
    ylabel(, labsize(medlarge) angle(0) nogrid) ///
    ytitle("Percent (100 × log)", size(medsmall) margin(small)) ///
    graphregion(color(white)) ///
    plotregion(color(white))           ///
    title("Stage 2: Inverse-propensity Weighted Local Projection", ///
          color(black) size(large) margin(medium)) ///
    subtitle(, size(small))             ///
    legend(off)                         ///
    xsize(3) ysize(3)                   ///
    name(stage2, replace)

*Figure 5
gr combine stage1 stage2, rows(1) iscale(0.75) graphregion(color(white) margin(l=0 r=3 t=1)) imargin(0 0) xsize(10)  ysize(4.25) ///
title("Old Age Dependency Ratio after Populist Comes to Power: IPWRA-LPs" , ///
          size(large) margin(medium))

gr export ${figures}/Figure5_old.pdf, replace

*Table C3
esttab IPW* using ${tables}/TableC3_old.tex, replace scalars(NonpPop_pdiff) se r2 keep(atakeover placebo) nonum compress b(2) se(2) sfmt(2) obslast nonotes label starlevels(* 0.1 ** 0.05 *** 0.01)

clear 
