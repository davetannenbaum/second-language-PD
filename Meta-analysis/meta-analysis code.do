** =======================================
** This file: meta-analysis.do
** Format: Stata 13 do-file
** Author: David Tannenbaum <david.tannenbaum@utah.edu>
** =======================================

** IMPORTANT: Need to set working directory to call on data files
** ---------------------------------------
snapshot erase _all
cd "~/GitHub/second-language-PD/"

** Combining data sets
** ---------------------------------------
use "meta-analysis/Experiment1.dta", clear
gen study = 1
append using "meta-analysis/Experiment2.dta", gen(study2)
replace study = 2 if study2 == 1
append using "meta-analysis/Experiment3.dta", gen(study3)
replace study = 3 if study3 == 1
append using "meta-analysis/Experiment4.dta", gen(study4)
replace study = 4 if study4 == 1
append using "meta-analysis/Experiment5.dta", gen(study5)
replace study = 5 if study5 == 1
append using "meta-analysis/Experiment6.dta", gen(study6)
replace study = 6 if study6 == 1
drop study2-study6
egen pid = group(study id)
drop id
rename pid id

// Traditional Utilitarian measure (analysis using only incongruent trials)
gen traditional = 1 - dv1

// Orthogonal contrasts
foreach v in U D {
	bysort study: egen mean = mean(`v')
	by study: egen sd = sd(`v')
	gen std_`v' = (`v' - mean) / sd
	drop mean sd
}
rename std_U d0
rename std_D d1
reshape long d, i(id) j(task)
rename d dv
egen cells = group(cond task)
recode cells (1 2 3 = 1) (4 = -3), gen(contrast1) // Blunted Deontology (BD)
recode cells (1 2 4 = -1) (3 = 3), gen(contrast2) // Heightened Utilitariansim (HU)
recode cells (3 = 1) (1 2 = 0) (4 = -1), gen(contrast3) // Hybrid Account (BD + HU)

// pruning data set and saving in memory
keep study id task cond dv U D traditional contrast1 contrast2 contrast3
order study id task cond dv U D traditional contrast1 contrast2 contrast3
snapshot save

** Summary of Results across Experiments
** ---------------------------------------
snapshot restore 1
collapse D U traditional, by(id cond study)
table study cond, c(mean D sd D) format(%9.2f)
table study cond, c(mean U sd U) format(%9.2f)
table study cond, c(mean traditional sd traditional) format(%9.2f)

** Meta-analysis of differences in D, U, and traditional U scores
** ---------------------------------------
snapshot restore 1
foreach var of varlist D U traditional {
	snapshot restore 1
	statsby r(N_1) r(mu_1) r(sd_1) r(N_2) r(mu_2) r(sd_2), by(study) verbose nodots clear: ttest `var', by(cond)
	metan _stat_1 _stat_2 _stat_3 _stat_4 _stat_5 _stat_6, cohen random nograph
}

** Meta-analysis of Orthogonal Contrasts
** ---------------------------------------
forvalues i = 1/3 {
	snapshot restore 1
	statsby _b _se, by(study) verbose nodots clear: regress dv contrast`i', cluster(id)
	metan _b_contrast`i' _se_contrast`i', random nograph
}

** Robustness Tests
** ---------------------------------------
snapshot restore 1

// native language
regress dv contrast1 if inlist(study,1,4,5)
estimates store germanL1
regress dv contrast1 if inlist(study,2,6)
estimates store englishL1
regress dv contrast1 if inlist(study,3)
estimates store spanishL1
suest germanL1 englishL1 spanishL1, cluster(id)
test [germanL1_mean = englishL1_mean]:contrast1
test [englishL1_mean = spanishL1_mean]:contrast1
test [germanL1_mean = spanishL1_mean]:contrast1

// foreign language
regress dv contrast1 if inlist(study,6)
estimates store germanL2
regress dv contrast1 if inlist(study,1,3,4,5)
estimates store englishL2
regress dv contrast1 if inlist(study,2)
estimates store spanishL2
suest germanL2 englishL2 spanishL2, cluster(id)
test [germanL2_mean = englishL2_mean]:contrast1
test [englishL2_mean = spanishL2_mean]:contrast1
test [germanL2_mean = spanishL2_mean]:contrast1

// crossedv languages
regress dv contrast1 if inlist(study,2)
estimates store cross1a
regress dv contrast1 if inlist(study,3)
estimates store cross1b
regress dv contrast1 if inlist(study,6)
estimates store cross2a
regress dv contrast1 if inlist(study,1,4,5)
estimates store cross2b
suest cross1a cross1b, cluster(id)
test [cross1a_mean = cross1b_mean]:contrast1
suest cross2a cross2b, cluster(id)
test [cross2a_mean = cross2b_mean]:contrast1

// elicitation format
regress dv contrast1 if inlist(study,1,2) 
estimates store judgment
regress dv contrast1 if inlist(study,3,4)
estimates store judgmentconseq
regress dv contrast1 if inlist(study,5,6)
estimates store choice
suest judgment judgmentconseq choice, cluster(id)
test [judgment_mean = judgmentconseq_mean]:contrast1
test [judgment_mean = choice_mean]:contrast1
test [judgmentconseq_mean = choice_mean]:contrast1

// dilemma set
regress dv contrast1 if inlist(study,1,2) 
estimates store dilemmaset1
regress dv contrast1 if inlist(study,3,4,5,6)
estimates store dilemmaset2
suest dilemmaset1 dilemmaset2, cluster(id)
test [dilemmaset1_mean = dilemmaset2_mean]:contrast1