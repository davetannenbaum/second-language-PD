** =======================================
** This file: Experiment5.do
** Format: Stata 13 do-file
** Author: David Tannenbaum <david.tannenbaum@utah.edu>
** =======================================

** IMPORTANT: Need to set working directory to call on data files
** ---------------------------------------
snapshot erase _all
cd "~/GitHub/second-language-PD/"
import delimited "Experiment5/Experiment5 raw.csv", varnames(1) case(preserve) clear

** Clean Up
** ---------------------------------------
drop in 1
quietly destring, replace

// filtering subjects
keep if Include == 1
gen id = _n

// renaming variables
rename V5 cond
replace cond = cond - 1
label define condl 0 "L1" 1 "L2"
label val cond condl
rename age_1 age
quietly destring, replace

// language proficiency
alpha Q55_1 Q55_2 Q55_3 Q55_4, gen(l2_proficiency)
alpha Q55_5 Q55_6 Q55_7 Q55_8, gen(l1_proficiency)

// reshaping data
reshape long dv, i(id) j(trial)

// omitting "I don't understand" responses and recoding dv (0 = no, 1 = yes)
drop if dv == 3
replace dv = 0 if dv == 2

// generating for incongruent dilemmas
gen incongruent = mod(trial,2)

// generating senario pairs
gen scenario = trial/2 if incongruent == 0
replace scenario = (trial + 1)/2 if incongruent == 1

// pruning data set
keep id cond trial scenario incongruent dv age gender l1 l2
order id cond trial scenario incongruent dv age gender l1 l2

// Creating U and D scores
replace dv = 1 - dv
separate dv, by(incongruent)
collapse dv0 dv1 l1 l2, by(id cond gender)
gen U = dv0 - dv1
gen D = dv1/(1 - U)
drop if U == 1

// saving data
snapshot save
save "meta-analysis/Experiment5.dta", replace
export delimited "Experiment5/Experiment5 clean.csv", replace

** Analysis
** ---------------------------------------
// Testing U and D scores across conditions
snapshot restore 1
ttest U, by(cond)
esize twosample U, by(cond)
ttest D, by(cond)
esize twosample D, by(cond)

// Traditional Utilitarian measure (analysis using only incongruent trials)
snapshot restore 1
gen traditional = 1 - dv1
ttest traditional, by(cond)
esize twosample traditional, by(cond)

// Orthogonal contrasts
snapshot restore 1
egen d0 = std(U)
egen d1 = std(D)
reshape long d, i(id) j(task)
egen cells = group(cond task)
recode cells (1 2 3 = 1) (4 = -3), gen(contrast1) // Blunted Deontology (BD)
recode cells (1 2 4 = -1) (3 = 3), gen(contrast2) // Heightened Utilitariansim (HU)
recode cells (3 = 1) (1 2 = 0) (4 = -1), gen(contrast3) // Hybrid Account (BD + HU)
regress d contrast1, cluster(id)
regress d contrast2, cluster(id)
regress d contrast3, cluster(id)

** Analysis in Supplmental Materials
** ---------------------------------------
// Correlations among U, D and Traditional U parameters
snapshot restore 1
gen traditional = 1 - dv1
collapse U D traditional, by(id)
pwcorr U D traditional, sig

// Gender effects
snapshot restore 1
ttest U, by(gender)
ttest D, by(gender)
gen traditional = 1 - dv1
collapse traditional, by(id gender)
ttest traditional, by(gender)

// Proficiency effects
snapshot restore 1
regress U c.l2_proficiency, robust
regress U i.cond##c.l2_proficiency, robust
regress D c.l2_proficiency, robust
regress D i.cond##c.l2_proficiency, robust
egen d0 = std(U)
egen d1 = std(D)
reshape long d, i(id) j(task)
egen cells = group(cond task)
recode cells (1 2 3 = 1) (4 = -3), gen(contrast1) // Blunted Deontology (BD)
recode cells (1 2 4 = -1) (3 = 3), gen(contrast2) // Heightened Utilitariansim (HU)
recode cells (3 = 1) (1 2 = 0) (4 = -1), gen(contrast3) // Hybrid Account (BD + HU)
regress d c.contrast1##c.l2_proficiency, cluster(id)
regress d c.contrast2##c.l2_proficiency, cluster(id)
regress d c.contrast3##c.l2_proficiency, cluster(id)