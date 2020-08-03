** =======================================
** This file: Experiment1.do
** Format: Stata 13 do-file
** Author: David Tannenbaum <david.tannenbaum@utah.edu>
** =======================================

** IMPORTANT: Need to set working directory to call on data files
** ---------------------------------------
snapshot erase _all
cd "~/GitHub/second-language-PD/"
import delimited "Experiment1/Experiment1 raw.csv", varnames(1) case(preserve) clear

** Clean Up
** ---------------------------------------
drop in 1
quietly destring, replace

// filtering subjects
keep if Include == "Yes"

// renaming variables
// note: requires 'encoder' program from SSC
rename V5 cond
encoder cond, replace
replace cond = 0 if cond == 2
label define condl 0 "L1" 1 "L2"
label val cond condl
rename v73 dv4
rename v83 dv14
rename age_1 age

// constructing individual difference items
forvalues i = 1/5 {
  replace answerCRT`i' = lower(answerCRT`i')
  replace answerCRT`i' = itrim(answerCRT`i')
  replace answerCRT`i' = trim(answerCRT`i')
  replace answerCRT`i' = subinstr(answerCRT`i', " ","",.)
}

// rescoring for subject who answered correctly
replace answerCRT1 = "4" in 158
gen crt1 = inlist(answerCRT1,"4")
gen crt2 = inlist(answerCRT2,"29")
gen crt3 = inlist(answerCRT3,"no")
gen crt4 = inlist(answerCRT4,"2.25","2.25dollar") // crt is scored incorrectly here, so should omit crt4
gen crt5 = inlist(answerCRT5,"5")
revrs IRI_2, replace
revrs IRI_5, replace
revrs IRI_6, replace
alpha IRI*, item gen(IRI)
revrs Q29_1, replace
revrs Q29_2, replace
revrs Q29_5, replace
alpha Q29*, item gen(NFC)
egen CRT = rowtotal(crt*)

// language proficiency
alpha Q55_1 Q55_2 Q55_3 Q55_4, gen(l2_proficiency)
alpha Q55_5 Q55_6 Q55_7 Q55_8, gen(l1_proficiency)

// reshaping data
gen id = _n
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
keep id cond trial scenario incongruent dv IRI NFC CRT age gender l1_proficiency l2_proficiency
order id cond trial scenario incongruent dv IRI NFC CRT age gender l1_proficiency l2_proficiency

// Creating U and D scores
replace dv = 1 - dv
separate dv, by(incongruent)
collapse dv0 dv1 IRI NFC CRT, by(id cond gender l1 l2)
gen U = dv0 - dv1
gen D = dv1/(1 - U)
drop if U == .
drop if D == .

// saving data
snapshot save
save "meta-analysis/Experiment1.dta", replace
export delimited "Experiment1/Experiment1 clean.csv", replace

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

// Individual difference results
snapshot restore 1
regress D IRI NFC CRT, robust
estimates store m1, title(Deontology)
regress U IRI NFC CRT, robust
estimates store m2, title(Utilitarianism)
estout m1 m2, cells(b(star fmt(3)) se(par fmt(2))) legend label varlabels(_cons Constant) stats(r2, label(R-sqr))

// Individual differences across conditions
snapshot restore 1
tabstat IRI NFC CRT, by(cond) stats(mean sem)format(%9.2f)
ttest IRI, by(cond)
esize twosample IRI, by(cond)
ttest NFC, by(cond)
esize twosample NFC, by(cond)
ttest CRT, by(cond)
esize twosample CRT, by(cond)

// Mediation
snapshot restore 1
sem (IRI <- cond) (NFC <- cond) (CRT <- cond) (U <- IRI NFC CRT cond) (D <- IRI NFC CRT cond), vce(robust) nocapslatent
estat teffects
matrix bi = r(indirect)
matrix bd = r(direct)
matrix bt = r(total)
display "Proportion of total effect on U explained by varlist(IRI, NFC, CRT) = " bi[1,7]/bt[1,7]
display "Proportion of indirect effect on U explained by IRI = " (bd[1,1] * bd[1,4])/bi[1,7]
display "Proportion of indirect effect on U explained by NFC = " (bd[1,2] * bd[1,5])/bi[1,7]
display "Proportion of indirect effect on U explained by CRT = " (bd[1,3] * bd[1,6])/bi[1,7]
display "Proportion of total effect on D explained by varlist(IRI, NFC, CRT) = " bi[1,11]/bt[1,11]
display "Proportion of indirect effect on D explained by IRI = " (bd[1,1] * bd[1,8])/bi[1,11]
display "Proportion of indirect effect on D explained by NFC = " (bd[1,2] * bd[1,9])/bi[1,11]
display "Proportion of indirect effect on D explained by CRT = " (bd[1,3] * bd[1,10])/bi[1,11]

// Bootstrapped mediation (indirect1: DV = U) (indirect2: DV = D)
snapshot restore 1
program drop _all
program mediation, rclass
  sem (IRI <- cond) (NFC <- cond) (CRT <- cond) (U <- IRI NFC CRT cond) (D <- IRI NFC CRT cond), vce(robust) nocapslatent
  return scalar path_a = _b[IRI:cond]
  return scalar path_b = _b[NFC:cond]
  return scalar path_c = _b[CRT:cond]
  return scalar path_d = _b[U:cond]
  return scalar path_e = _b[D:cond]
  return scalar path_f = _b[U:IRI]
  return scalar path_g = _b[U:NFC]
  return scalar path_h = _b[D:NFC]
  return scalar path_i = _b[D:CRT]
  return scalar path_j = _b[U:CRT]
  return scalar path_k = _b[D:IRI]
  return scalar indirectD_IRI = _b[IRI:cond] * _b[D:IRI]
  return scalar indirectD_NFC = _b[NFC:cond] * _b[D:NFC]
  return scalar indirectD_CRT = _b[CRT:cond] * _b[D:CRT]
  return scalar indirectD_all = (_b[IRI:cond] * _b[D:IRI]) + (_b[NFC:cond] * _b[D:NFC]) + (_b[CRT:cond] * _b[D:CRT])
  return scalar indirectU_IRI = _b[IRI:cond] * _b[U:IRI]
  return scalar indirectU_NFC = _b[NFC:cond] * _b[U:NFC]
  return scalar indirectU_CRT = _b[CRT:cond] * _b[U:CRT]
  return scalar indirectU_all = (_b[IRI:cond] * _b[U:IRI]) + (_b[NFC:cond] * _b[U:NFC]) + (_b[CRT:cond] * _b[U:CRT])
end
bootstrap r(path_a) r(path_b) r(path_c) r(path_d) r(path_e) r(path_f) r(path_g) r(path_h) r(path_i) r(path_j) r(path_k) r(indirectD_IRI) r(indirectD_NFC) r(indirectD_CRT) r(indirectD_all) r(indirectU_IRI) r(indirectU_NFC) r(indirectU_CRT) r(indirectU_all), reps(5000): mediation
estat bootstrap, percentile bc

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