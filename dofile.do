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

twoway kdensity tprofits_b if dummy_b==0, xtitle("Profit (Ksh)") title("Profit distribution -kernel- by treatment") color(blue*.5) lcolor(blue)|| kdensity tprofits_b if dummy_b==1, color(red*.5) lcolor(red) || kdensity tprofits_b, color(green*.5) lcolor(green) legend(order(1 "semi-rural women" 2 "rural women")) legend(order(1 "Control" 2 "Class Treat." 3 "Mentor Treat.") col(1) pos(1) ring(0))

** histograms

twoway histogram tprofits_b if dummy_b==0, xtitle("Profit (Ksh)") title("Profit distribution -kernel- for each treatment") color(blue%30) lcolor(blue)|| histogram tprofits_b if dummy_b==1, color(red%30) lcolor(red) || histogram tprofits_b, color(green%30) lcolor(green) legend(order(1 "semi-rural women" 2 "rural women")) legend(order(1 "Control" 2 "Class Treat." 3 "Mentor Treat.") col(1) pos(1) ring(0))

/// no se ve nada

** separated histograms
histogram tprofits, by(dummy, cols(3),title("Profit distribution -histograms- by treatment"))


