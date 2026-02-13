//	AUTHOR:			GIUSEPPE FAÈ
//	CREATED:		APRIL 1, 2025
//	MODIFIED: 		APRIL 2, 2025
//	DESCRIPTION:	PRELIMINARY EVIDENCE

********************************************************************************
//		CLEAN UNIETD NATIONS DATA
********************************************************************************

// UN: DEMOGRAPHIC INDICATORS
local demographics WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT
import excel using ${un}/`demographics'.xlsx, cellrange(A17:BM22000) firstrow clear

	#delimit;
	keep Regionsubregioncountryorar
		 ISO3Alphacode
		 Type
		 Year
		 TotalPopulationasof1Januar
		 TotalPopulationasof1July
		 MalePopulationasof1Julyt
		 FemalePopulationasof1July
		 PopulationSexRatioasof1Ju
		 MedianAgeasof1Julyyears
		 Birthsbywomenaged15to19t
		 TotalFertilityRatelivebirth
		 NetNumberofMigrantsthousand
		 NetMigrationRateper1000po
	;
	#delimit cr

	// Keep only observations for countries
	keep if Type == "Country/Area"
	drop Type

	// Change variable format
	#delimit;
	destring TotalPopulationasof1Januar
			 TotalPopulationasof1July
			 MalePopulationasof1Julyt
			 FemalePopulationasof1July
			 PopulationSexRatioasof1Ju
			 MedianAgeasof1Julyyears
			 Birthsbywomenaged15to19t
			 TotalFertilityRatelivebirth
			 NetNumberofMigrantsthousand
			 NetMigrationRateper1000po,
	  replace ignore(",") force;
	#delimit cr
		
	// HOUSE-KEEPING
	sort Regionsubregioncountryorar Year
	
	#delimit ;
	rename (TotalPopulationasof1Januar TotalPopulationasof1July 
			MalePopulationasof1Julyt FemalePopulationasof1July
			PopulationSexRatioasof1Ju MedianAgeasof1Julyyears 
			Birthsbywomenaged15to19t TotalFertilityRatelivebirth
			NetNumberofMigrantsthousand NetMigrationRateper1000po)
			(pop_jan pop_jul male female pop_sex med_age birth_age fertility
			 migration migration_rate);
	#delimit cr
	
	tempfile temp_demoindic
	save `temp_demoindic'

// UN SHARES BY BROAD AGE
local age WPP2024_POP_F02_1_POPULATION_5-YEAR_AGE_GROUPS_BOTH_SEXES
import excel using ${un}/`age'.xlsx, cellrange(A17:AF22000) firstrow clear

	drop Index Variant Notes Locationcode ISO2Alphacode SDMXcode Parentcode
	
	// Keep only observations for countries
	keep if Type == "Country/Area"
	drop Type
	
	// Change variable format
	#delimit;
	destring L M N O P Q R S T U V W X Y Z AA AB AC AD AE AF,
		replace ignore(",") force;
	#delimit cr
	
	// HOUSE-KEEPING
	sort Regionsubregioncountryorar Year
	
	local letters L M N O P Q R S T U V W X Y Z AA AB AC AD AE AF
	#delimit ;
	local agegroups   age0_4   age5_9   age10_14 age15_19 age20_24
                      age25_29 age30_34 age35_39 age40_44 age45_49
			          age50_54 age55_59 age60_64 age65_69 age70_74
                      age75_79 age80_84 age85_89 age90_94 age95_99 age100
	;
	#delimit cr
	forval i = 1/21 {
		local old : word `i' of `letters'
		local new : word `i' of `agegroups'
		rename `old' `new'
	}
	
// Merge demographic data
#delimit;
merge 1:1 Regionsubregioncountryorar Year using `temp_demoindic',
	assert(match) nogen
	;
rename (Regionsubregioncountryorar ISO3Alphacode Year)
	   (country iso year);	
#delimit cr
// Sort and save
sort country year
order country iso year, first

// Generate OLD AGE DEPENDENCY RATIO
#delimit ;
gen pop_65plus = age65_69 + age70_74 + age75_79 + age80_84 + age85_89 + 
                 age90_94 + age95_99 + age100;

gen pop_15to64 = age15_19 + age20_24 + age25_29 + age30_34 + age35_39 + 
                 age40_44 + age45_49 + age50_54 + age55_59 + age60_64;
#delimit cr

gen oldage_ratio = (pop_65plus / pop_15to64) * 100
drop pop_65plus pop_15to64

compress
save ${data_clean}/demographics.dta, replace
clear
********************************************************************************
// MERGE WITH ple_dataset BY FUNKE, SCHULARICK, & TREBESCH (2023)
********************************************************************************
use ${funke}/ple_dataset, clear

merge 1:1 iso year using ${data_clean}/demographics.dta, nogen
drop if missing(cid) // To remain with the 60 original countries

********************************************************************************
// CREATE POST_3 VARIABLE
********************************************************************************
xtset cid year

// Create lagged takeover indicators for 1 to 3 years
forvalues h = 1/3 {
    gen ptakeover`h' = (l`h'.atakeover)
}

// Generate Post_3 as sum of 3 years after populist takeover
gen Post_3 = ptakeover1 + ptakeover2 + ptakeover3

// Handle edge cases
replace Post_3 = 1 if Post_3 == 2 | Post_3 == 3
replace Post_3 = 0 if Post_3 == . & atakeover == 0

// Clean up temporary variables
drop ptakeover*

********************************************************************************
// FINAL-HOUSEKEEPING
********************************************************************************
// Order Post_3
order Post_3, before(Post_5)
// Label variables
label var oldage_ratio "Old-age dependency ratio (65+/15-64)"
label variable Post_3 "3-year aftermath of populist government takeover (0-1 dummy), core cases"
label variable migration "#immigrants – #emigrants"

forvalues i = 0(5)95 {
    local j = `i' + 4
    label variable age`i'_`j' "Tot. population aged `i'-`j' (both sexes, thousands)"
}
label variable age100 "Tot. population aged 100+ (both sexes, thousands)"


// Erase intermediary files	
erase ${data_clean}/demographics.dta

// Save
compress
save ${data_clean}/populist_demographics.dta, replace

clear
