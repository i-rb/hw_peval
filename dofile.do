*********************************
** HOMEWORK PROGRAM EVALUATION **
*********************************

*************** 0. PRELIMINARS. **************************

clear all
set more off
set seed 33000

cd "/Users/ivanrendobarreiro/Documents/GitHub/hw_program_eval/"
use "rct_kenya.dta"

*************** 4. BALANCING TEST ************************

* controls to be used: secondary education, age of the owner, and business sector fixed effects (???) , and an indicator for whether the firm employs any workers 

* (!) data issue: if i is in control, treatment variable for i is not 0, but . (nan))

replace treat_class_b = 0 if missing(treat_class_b) // technical issue with nans, then collinearity
replace treat_mentor_b= 0 if missing(treat_mentor_b) // technical issue with nans, then collinearity

** different regressions (ttest included)

*** (i) secondary school 

reg secondaryedu_b treat_class_b treat_mentor_b if baseline==1


*** (ii) age

reg age_b treat_class_b treat_mentor_b if baseline==1


*** (iii) retail sector

reg sec0_b treat_class_b treat_mentor_b if baseline==1


*** (iv) manufacturing sector

reg sec1_b treat_class_b treat_mentor_b if baseline==1


*** (v) services sector

reg sec2_b treat_class_b treat_mentor_b if baseline==1


*** (vi) food prep sector

reg sec3_b treat_class_b treat_mentor_b if baseline==1


*** (vii) other sector

reg sec4_b treat_class_b treat_mentor_b if baseline==1


*** (viii) has employees? dummy

reg I_emp_b treat_class_b treat_mentor_b if baseline==1


*************** 5. TREATMENT EFFECTS ************************

* 1. Graph the profit distribution 

** it is better to have a dummy with 0=control, 1=class, 2=mentor, so we create it

gen dummy_b= treat_class_b + 2*treat_mentor_b

** kernel distribution 

///twoway kdensity tprofits_b if dummy_b==0, xtitle("Profit (Ksh)") title("Profit distribution -kernel- by treatment") color(blue*.5) lcolor(blue)|| kdensity tprofits_b if dummy_b==1, color(red*.5) lcolor(red) || kdensity tprofits_b, color(green*.5) lcolor(green) legend(order(1 "semi-rural women" 2 "rural women")) legend(order(1 "Control" 2 "Class Treat." 3 "Mentor Treat.") col(1) pos(1) ring(0))

** histograms

/// twoway histogram tprofits_b if dummy_b==0, xtitle("Profit (Ksh)") title("Profit distribution -kernel- for each treatment") color(blue%30) lcolor(blue)|| histogram tprofits_b if dummy_b==1, color(red%30) lcolor(red) || histogram tprofits_b, color(green%30) lcolor(green) legend(order(1 "semi-rural women" 2 "rural women")) legend(order(1 "Control" 2 "Class Treat." 3 "Mentor Treat.") col(1) pos(1) ring(0))


** separated histograms
/// histogram tprofits, by(dummy, cols(3))

** the graph we need
lgraph tprofits wave, by(dummy_b)


* 2. ATE replicating table 3

** Wave by wave regressions: 
**** y_it = \alpha_t + M_i\beta_t + X_i\gamma_t + y_i0\delta_t + X_i\eta_t + e_it
**** for each t. (They are as independent regressions)

****** t=1 

reg tprofits i.dummy_b lage_b secondaryedu_b sec0_b sec1_b sec2_b sec3_b sec4_b I_emp_b tprofits_b if wave == 1, robust

test 1.dummy_b = 2.dummy_b

****** t=2

reg tprofits i.dummy_b lage_b secondaryedu_b sec0_b sec1_b sec2_b sec3_b sec4_b I_emp_b tprofits_b if wave == 2, robust

test 1.dummy_b = 2.dummy_b

****** t=3 

reg tprofits i.dummy_b lage_b secondaryedu_b sec0_b sec1_b sec2_b sec3_b sec4_b I_emp_b tprofits_b if wave == 3, robust

test 1.dummy_b = 2.dummy_b

****** t=4 

reg tprofits i.dummy_b lage_b secondaryedu_b sec0_b sec1_b sec2_b sec3_b sec4_b I_emp_b tprofits_b if wave == 4, robust

test 1.dummy_b = 2.dummy_b

****** t=5 

reg tprofits i.dummy_b lage_b secondaryedu_b sec0_b sec1_b sec2_b sec3_b sec4_b I_emp_b tprofits_b if wave == 5, robust

test 1.dummy_b = 2.dummy_b

****** t=6 

reg tprofits i.dummy_b lage_b secondaryedu_b sec0_b sec1_b sec2_b sec3_b sec4_b I_emp_b tprofits_b if wave == 6, robust

test 1.dummy_b = 2.dummy_b


****** t=7 

reg tprofits i.dummy_b lage_b secondaryedu_b sec0_b sec1_b sec2_b sec3_b sec4_b I_emp_b tprofits_b if wave == 7, robust

test 1.dummy_b = 2.dummy_b



** pooled regression

**** create variable for fixed effects 

gen w1 = 0
replace w1 = 1 if wave == 1

gen w2 = 0
replace w2 = 1 if wave == 2

gen w3 = 0
replace w3 = 1 if wave == 3

gen w4 = 0
replace w4 = 1 if wave == 4

gen w5 = 0
replace w5 = 1 if wave == 5

gen w6 = 0
replace w6 = 1 if wave == 6

gen w7 = 0
replace w7 = 1 if wave == 7

**** reg

replace dummy_b=0 if wave==0 /// at t=0, no treatment for any individual (all control)

/// drop extrema 1%

preserve

summarize tprofits, detail
drop if tprofits < r(p1) | tprofits > r(p99)


**** pooled reg without 1% extrema

reg tprofits i.dummy_b w1 w2 w3 w4 w5 w6 w7 lage_b secondaryedu_b sec0_b sec1_b sec2_b sec3_b sec4_b I_emp_b tprofits_b, robust

test 1.dummy_b = 2.dummy_b

restore 

**** pooled reg with all the data

reg tprofits i.dummy_b w1 w2 w3 w4 w5 w6 w7 lage_b secondaryedu_b sec0_b sec1_b sec2_b sec3_b sec4_b I_emp_b tprofits_b, robust

test 1.dummy_b = 2.dummy_b



*********** 6. EFFECT OF TREATMENTS ON "SWITCH SUPPLIER" AND "KEEP" RECORDS **************

** Linear Probability Model for both cases (without controlling)

reg supplierswitch i.dummy_b if wave==5, robust

reg keeps_some_records i.dummy_b if wave==6, robust 

** Linear Probability Model for both cases (with controls)

reg supplierswitch i.dummy_b lage_b secondaryedu_b sec0_b sec1_b sec2_b sec3_b sec4_b I_emp_b tprofits_b if wave==5, robust

reg keeps_some_records i.dummy_b formalaccount_b lage_b sec0_b sec1_b sec2_b sec3_b sec4_b secondaryedu_b I_emp_b tprofits_b if wave==6, robust 




*******************************************************************************
*****************  SECOND PART
*******************************************************************************

clear 
use "rd_mentors.dta"

*********** 7. BALANCING TEST FOR THE CONTROL VARIABLES ***********************

** We replicate Question 4 regressions here, but changing the variables

*** (i) ce_std

reg treat ce_std, robust 


