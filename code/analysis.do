
/*==========================================================================

Title: analysis.do 

Description: Replication of Stata tables and estimates for the gender
interactions paper.

Usage: from the code folder, run
    do analysis.do [../data/my_data_simulated.csv] [../output]

If no arguments are supplied, the script reads GENDER_DATA_CSV and
GENDER_OUTPUT_DIR environment variables. If those are not set, it expects
the input data at ../data/my_data_simulated.csv and writes outputs to
../output.

===========================================================================*/

version 19
clear all 
set more off
capture log close

args data_csv output_dir

if `"`data_csv'"' == "" {
	local data_csv : environment GENDER_DATA_CSV
}
if `"`data_csv'"' == "" {
	local data_csv "../data/my_data_simulated.csv"
}

if `"`output_dir'"' == "" {
	local output_dir : environment GENDER_OUTPUT_DIR
}
if `"`output_dir'"' == "" {
	local output_dir "../output"
}

capture mkdir "`output_dir'"
log using "`output_dir'/analysis.log", replace text

foreach cmd in eststo esttab reghdfe coefplot ivreghdfe {
	capture which `cmd'
	if _rc {
		di as error "Required Stata command not found: `cmd'"
		di as error "Install the required package before running this do-file."
		exit 199
	}
}

eststo clear 

capture confirm file "`data_csv'"
if _rc {
	di as error "Input data not found: `data_csv'"
	di as error "Pass the CSV path as the first argument or set GENDER_DATA_CSV."
	exit 601
}

di as text "Input data: `data_csv'"
di as text "Output directory: `output_dir'"

import delimited using "`data_csv'", clear


loc detail = 1 // set this to 1 to hide regression output

if `detail' == 1 {
	loc qui "qui:"
}



* ===================================================================== *
* -----------------     	Preliminary cleaning        ----------------- *
* ===================================================================== *

* students

#delimit ;
local studlist "age female reservation_stu father_college mother_college b_i_jeemain_score 
			b_math_g1_score b_physics_g1_score e_math_g3_score e_physics_g3_score b_ct_score b_ql_score 
			b_math_confidence b_physics_confidence e_math_confidence e_physics_confidence b_math_anxiety 
			b_physics_anxiety e_math_anxiety e_physics_anxiety b_stay_branch e_stay_branch e_received_tutoring  
			b_salary_expected e_salary_expected  b_n_friends e_n_friends e_ask_questions2 b_ask_questions2 
			you_equityminded_math you_equityminded_physics mates_equityminded_math mates_equityminded_physics
			e_mathstudy_25 b_mathstudy_25 e_physicsstudy_25 b_physicsstudy_25 e_mathstudy_27 b_mathstudy_27 
			e_physicsstudy_27 b_physicsstudy_27 e_mathstudy_29 b_mathstudy_29 e_physicsstudy_29 b_physicsstudy_29";
#delimit cr

foreach s in `studlist' {
	gen `s'_r = `s' 
	`qui' replace  `s'_r = "" if `s'_r == "NA"

	gen `s'_miss = mi(`s'_r)

	`qui' destring `s'_r, replace

	gen `s'_imp = `s'_r
	replace `s'_imp = 0 if `s'_miss == 1

}





* faculty
loc faclist "fac_associate_professor fac_assistant_professor fac_professor fac_yearsinhighed fac_degree_college_elite reservation_fac fac_age" 


foreach s in `faclist' {
	gen `s'_r = `s' 
	`qui' replace  `s'_r = "" if `s'_r == "NA"
	`qui' destring `s'_r, replace

	gen `s'_miss = mi(`s'_r)

}


* cases where the variable names are very long
gen fac_phd_r = fac_highest_degree_phd
gen fac_phd_in_prog_r = fac_highest_degree_phd_in_prog

gen fac_phdprog_master = fac_phd_in_prog_r == 1  |  fac_highest_degree_masters == 1

* ===================================================================== *
* -----------------     	      Fixed effects         ----------------- *
* ===================================================================== *

* course fixed effects
gen coursefe = department_id + course_name

* classroom fixed effects
gen classfe = coursefe + facid


* ===================================================================== *
* -----------------  Standardise course grades        ----------------- *
* ===================================================================== *

gen coursegrade = course_rank
replace coursegrade = "" if course_rank == "NA"
destring coursegrade, replace

gen std_coursegrade = . 

`qui' levelsof coursefe, loc(course)

foreach c in `course' {
	`qui' cap summ coursegrade if coursefe == "`c'"
	`qui' cap replace std_coursegrade  = (coursegrade - `r(mean)')/`r(sd)' if coursefe == "`c'"
}


* ===================================================================== *
* ----------    Ability, belonging, self-perceptions, etc ------------- *
* ===================================================================== *

* anxiety 
gen b_anxiety =  (b_math_anxiety_r + b_physics_anxiety_r)/2

`qui' summ b_anxiety, d
gen low_anxiety = b_anxiety < `r(p50)' 
replace low_anxiety = . if mi(b_anxiety)

gen std_b_anxiety = (b_anxiety - `r(mean)')/`r(sd)'

gen e_anxiety =  (e_math_anxiety_r + e_physics_anxiety_r)/2

`qui' summ e_anxiety
gen std_e_anxiety = (e_anxiety - `r(mean)')/`r(sd)'


* confidence
gen b_confidence =  (b_math_confidence_r + b_physics_confidence_r)/2

`qui' summ b_confidence, d
gen low_confidence = b_confidence < `r(p50)' 
replace low_confidence = . if mi(b_confidence)

gen std_b_confidence = (b_confidence - `r(mean)')/`r(sd)'

gen e_confidence =  (e_math_confidence_r + e_physics_confidence_r)/2

`qui' summ e_confidence
gen std_e_confidence = (e_confidence - `r(mean)')/`r(sd)'


* ability
gen b_acadskills = (b_math_g1_score_r + b_physics_g1_score_r)/2

`qui' summ b_acadskills, d
gen low_acadskills = b_acadskills < `r(p50)' 
replace low_acadskills = . if mi(b_acadskills)

gen std_b_acadskills = (b_acadskills - `r(mean)')/`r(sd)'


* belonging 
gen b_belonging  = b_stay_branch_r

gen low_belonging = b_belonging == 0
replace low_belonging = . if mi(b_belonging)


gen belonging_e_r = real(e_stem_belonging_z)

`qui' summ b_i_jeemain_score_r
gen std_b_jee = (b_i_jeemain_score_r - `r(mean)')/`r(sd)'



* JEE scores
`qui' summ belonging_e_r
gen std_belonging_e = (belonging_e_r - `r(mean)')/`r(sd)'




* ===================================================================== *
* -----------------   Teacher beliefs and mindsets    ----------------- *
* ===================================================================== *

* teacher beliefs after two years
gen majorbetter = major_better_after2yrs 
replace majorbetter = "" if major_better_after2yrs == "NA"

encode majorbetter , gen(better_response)

gen fac_stereotype = 1 if inlist(better_response,4,5) & fac_female == 0
replace fac_stereotype = 2 if inlist(better_response,1,2,3) & fac_female == 0
replace fac_stereotype = 3 if inlist(better_response,4,5) & fac_female == 1
replace fac_stereotype = 4 if inlist(better_response,1,2,3) & fac_female == 1

lab def facstereo 1 "Male traditional" 2 "Male equity-minded" 3 "Female traditional" 4 "Female equity-minded"
lab val fac_stereotype facstereo

tab fac_stereotype, gen(fac_stereodum)

gen femXmaleeq = female_r*fac_stereodum2
gen femXfemalest = female_r*fac_stereodum3
gen femXfemaleeq = female_r*fac_stereodum4


* teacher beliefs after two years
gen majorbetter_ms = math_science_better_after2yrs
replace majorbetter_ms = "" if major_better_after2yrs == "NA"

encode majorbetter_ms, gen(better_response_ms)

gen fac_stereotype_ms = 1 if inlist(better_response_ms,4,5) & fac_female == 0
replace fac_stereotype_ms = 2 if inlist(better_response_ms,1,2,3) & fac_female == 0
replace fac_stereotype_ms = 3 if inlist(better_response_ms,4,5) & fac_female == 1
replace fac_stereotype_ms = 4 if inlist(better_response_ms,1,2,3) & fac_female == 1

lab val fac_stereotype_ms facstereo

tab fac_stereotype_ms, gen(fac_stereodum_ms)

gen femXmaleeq_ms = female_r*fac_stereodum_ms2
gen femXfemalest_ms = female_r*fac_stereodum_ms3
gen femXfemaleeq_ms = female_r*fac_stereodum_ms4


* clean mindset data 
split intelligence_beliefs , p("::")

split intelligence_beliefs1 , p("&&")

gen fac_mindset_r = . 
replace fac_mindset_r = 0 if intelligence_beliefs12 == "strongly agree"
replace fac_mindset_r = 1 if intelligence_beliefs12 == "agree"
replace fac_mindset_r = 2 if intelligence_beliefs12 == "neither agree nor disagree"
replace fac_mindset_r = 3 if intelligence_beliefs12 == "disagree"
replace fac_mindset_r = 4 if intelligence_beliefs12 == "strongly disagree"

split intelligence_beliefs2, p("&&")

gen fac_mindset_learn_r = .
replace fac_mindset_learn_r = 0 if intelligence_beliefs22 == "strongly agree"
replace fac_mindset_learn_r = 1 if intelligence_beliefs22 == "agree"
replace fac_mindset_learn_r = 2 if intelligence_beliefs22 == "neither agree nor disagree"
replace fac_mindset_learn_r = 3 if intelligence_beliefs22 == "disagree"
replace fac_mindset_learn_r = 4 if intelligence_beliefs22 == "strongly disagree"

split intelligence_beliefs3, p("&&")

gen fac_mindset_control_r = .
replace fac_mindset_control_r  = 0 if intelligence_beliefs32 == "strongly agree"
replace fac_mindset_control_r  = 1 if intelligence_beliefs32 == "agree"
replace fac_mindset_control_r  = 2 if intelligence_beliefs32 == "neither agree nor disagree"
replace fac_mindset_control_r  = 3 if intelligence_beliefs32 == "disagree"
replace fac_mindset_control_r  = 4 if intelligence_beliefs32 == "strongly disagree"

drop intelligence_beliefs*


* growth mindset (direct elicitation)
gen fixed_mindset = fac_mindset_r ==  0 | fac_mindset_r == 1

replace fixed_mindset = . if fac_mindset_r == .

gen fac_fix = 1 if fixed_mindset == 0 & fac_female ==0
replace fac_fix = 2 if fixed_mindset == 1 & fac_female ==0
replace fac_fix = 3 if fixed_mindset == 0 & fac_female ==1
replace fac_fix = 4 if fixed_mindset == 1 & fac_female ==1

tab fac_fix , gen(fac_fix)

gen femXmale_fix = female_r*fac_fix2 // male fix
gen femXmale_gro = female_r*fac_fix1 // male gro
gen femXfem_fix = female_r*fac_fix4 // fem fix
gen femXfem_gro = female_r*fac_fix3 // fem gro


* teacher beliefs at the start of program
gen majorbetter_st = math_science_better_start
replace majorbetter_st  = "" if math_science_better_start == "NA"

encode majorbetter_st , gen(better_response_st )

gen fac_stereotype_st  = 1 if inlist(better_response_st ,4,5) & fac_female == 0
replace fac_stereotype_st  = 2 if inlist(better_response_st,1,2,3) & fac_female == 0
replace fac_stereotype_st  = 3 if inlist(better_response_st,4,5) & fac_female == 1
replace fac_stereotype_st  = 4 if inlist(better_response_st,1,2,3) & fac_female == 1

lab val fac_stereotype_st  facstereo

tab fac_stereotype_st, gen(fac_stereodum_st)

gen femXmaleeq_st = female_r*fac_stereodum_st2
gen femXfemalest_st = female_r*fac_stereodum_st3
gen femXfemaleeq_st = female_r*fac_stereodum_st4


* Mindset (skills can be learned, but intelligence is fixed)

gen fixed_mindsetl = fac_mindset_learn_r ==  0 | fac_mindset_learn_r == 1

replace fixed_mindsetl = . if fac_mindset_learn_r == .

gen fac_fixl = 1 if fixed_mindsetl == 0 & fac_female ==0
replace fac_fixl = 2 if fixed_mindsetl == 1 & fac_female ==0
replace fac_fixl = 3 if fixed_mindsetl == 0 & fac_female ==1
replace fac_fixl = 4 if fixed_mindsetl == 1 & fac_female ==1

tab fac_fixl , gen(fac_fixl)

gen femXmale_grol = female_r*fac_fixl1 // male gro
gen femXmale_fixl = female_r*fac_fixl2 // male fix
gen femXfem_grol = female_r*fac_fixl3 // fem gro
gen femXfem_fixl = female_r*fac_fixl4 // fem fix



* fac characteristics


gen fac_phdl = 1 if fac_phd_r  == 0 & fac_female ==0
replace fac_phdl  = 2 if fac_phd_r  == 1 & fac_female ==0
replace fac_phdl  = 3 if fac_phd_r  == 0 & fac_female ==1
replace fac_phdl  = 4 if fac_phd_r  == 1 & fac_female ==1

tab fac_phdl , gen(fac_phdl)

gen femXmale_nophdl = female_r*fac_phdl1 // male no phd
gen femXmale_phdl = female_r*fac_phdl2 // male phd
gen femXfem_nophdl = female_r*fac_phdl3 // fem no phd
gen femXfem_phdl = female_r*fac_phdl4 // fem phd


gen assoabv = fac_professor_r |fac_associate_professor_r 


gen fac_assl = 1 if assoabv  == 0 & fac_female ==0
replace fac_assl  = 2 if assoabv  == 1 & fac_female ==0
replace fac_assl  = 3 if assoabv  == 0 & fac_female ==1
replace fac_assl  = 4 if assoabv  == 1 & fac_female ==1

tab fac_assl , gen(fac_assl)

gen femXmale_noassl = female_r*fac_assl1 // male bel ass
gen femXmale_assl = female_r*fac_assl2 // male ass
gen femXfem_noassl = female_r*fac_assl3 // fem bel ass
gen femXfem_assl = female_r*fac_assl4 // fem ass



* ===================================================================== *
* -----------------   Teaching related practices      ----------------- *
* ===================================================================== *

*** weekly hours ***

ren weekly_hours_* wh_*

`qui' ds wh_tutoring wh_homework_tests wh_course wh_advising_students wh_lessons_planning wh_teaching_class

foreach s in `r(varlist)' {
	gen `s'_r = `s' 
	`qui' replace  `s'_r = "" if `s'_r == "NA"
	`qui' destring `s'_r, replace

	gen `s'_miss = mi(`s'_r)
}


*** TPI scores ***

`qui' summ tpi_score3
gen tpi_inclass = (tpi_score3 - `r(mean)')/`r(sd)'

`qui' summ tpi_score4
gen tpi_assignment = (tpi_score4 - `r(mean)')/`r(sd)'

`qui' summ tpi_score5
gen tpi_feedback = (tpi_score5 - `r(mean)')/`r(sd)'

`qui' summ tpi_score8
gen tpi_collab = (tpi_score8 - `r(mean)')/`r(sd)'


gen notstem = regexm(course_name, "ENGLISH") | regexm(course_name, "WRITING") ///
 | regexm(course_name, "YOGA") | regexm(course_name, "SOFT SKILLS") | ///
 regexm(course_name , "PROFESSIONAL COMMUNICATION") | regexm(course_name , "PHYSICAL EDUCATION") | ///
 regexm(course_name , "LANGUAGE LAB") |  regexm(course_name , "INTERPERSONAL SKILL") | ///
  regexm(course_name , "HUMAN VALUE") | regexm(course_name, "FOREIGN LANGUAGE") | ///
  regexm(course_name , "APTITUDE")  | regexm(course_name , "TECHNICAL COMMUNICATION") 

* ===================================================================== *
* -----------------     	  Class size weights        ----------------- *
* ===================================================================== *

bys classfe: egen class_size = count(stdid)
gen class_weight = 1 / class_size


* ===================================================================== *
* -----------------      Main interaction term        ----------------- *
* ===================================================================== *


gen femXfac_fem = female_r*fac_fem  // female X female faculty


* ===================================================================== *
* ----------------- Save processed data as tempfile   ----------------- *
* ===================================================================== *

tempfile full
save `full'


* ===================================================================== *
* -----------------     	 Academic outcomes        ----------------- *
* ===================================================================== *

lab var femXfac_fem "Course grades"

eststo t11: `qui' reghdfe std_coursegrade femXfac_fem , a(stdid classfe) vce(clus facid)

*collapse *_r age_miss fac_female, by(stdid subject department_id elite)


collapse std_* *_r *_miss *_imp fac_female low_* b_belonging, by(stdid subject department_id elite)


* keep only math and physics observations for students 
keep if inlist(subject, "math", "physics")

*** Processing math and science standardized test variables ***
rename b_math_g1_score_imp g1mat
rename b_physics_g1_score_imp g1phy
rename e_math_g3_score_r	 g3mat
rename e_physics_g3_score_r g3phy

* now all standardized physics and math scores will have the corresponding subject name (or the faculty)
gen g3 = g3mat
replace g3 = g3phy if subject == "physics"

gen g1 = g1mat
replace g1 = g1phy if subject == "physics"

gen std_g3 =.
gen std_g1 =.

`qui' summ g3 if subject == "math"
replace std_g3  = (g3 - `r(mean)')/`r(sd)' if subject == "math"

`qui' summ g3 if subject == "physics"
replace std_g3  = (g3 - `r(mean)')/`r(sd)' if subject == "physics"

`qui' summ g1 if subject == "math"
replace std_g1  = (g1 - `r(mean)')/`r(sd)' if subject == "math"

`qui' summ g1 if subject == "physics"
replace std_g1  = (g1 - `r(mean)')/`r(sd)' if subject == "physics"


gen g1_miss = .
replace g1_miss = b_physics_g1_score_miss if subject == "physics"
replace g1_miss = b_math_g1_score_miss if subject == "math"


* rescaling the fac_female variable
replace fac_female = fac_female*10
gen femXfac_female = female_r*fac_female

tempfile stdscores
save `stdscores'
 
lab var femXfac_female "Standardized test scores" 

eststo t12: `qui' reghdfe std_g3 female_r fac_female femXfac_female age_imp reservation_stu_imp father_college_imp ///
	mother_college_imp fac_age_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r ///
	fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r ///
	std_g1 ///
	g1_miss age_miss female_miss reservation_stu_miss father_college_miss mother_college_miss , ///
	a(department_id sub) vce(clus stdid)


esttab t1*, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ci(fmt(%9.3f) par) p(fmt(%9.2f)) ) starlevels(* 0.1 ** 0.05 *** 0.01)



* ===================================================================== *
* -----------------     STEM anxiety and confidence   ----------------- *
* ===================================================================== *

use `full', clear 

keep if inlist(subject, "math", "physics")

collapse *_r *_imp *_miss fac_female std_?_anxiety std_?_confidence low_* b_belonging, by(stdid department_id subject)

foreach x in e {
	foreach y in math physics {
		foreach z in anxiety confidence {
			summ `x'_`y'_`z'_r if subject == "`y'"
			gen std_`x'_`y'_`z' = (`x'_`y'_`z'_r - `r(mean)')/`r(sd)' if subject == "`y'"

		}
	}
}

foreach x in b {
	foreach y in math physics {
		foreach z in anxiety confidence {
			summ `x'_`y'_`z'_imp if subject == "`y'"
			gen std_`x'_`y'_`z' = (`x'_`y'_`z'_imp - `r(mean)')/`r(sd)' if subject == "`y'"

		}
	}
}


gen std_e_conf_stack = std_e_math_confidence
replace std_e_conf_stack = std_e_physics_confidence if subject == "physics"

gen std_e_anx_stack = std_e_math_anxiety
replace std_e_anx_stack = std_e_physics_anxiety if subject == "physics"



gen std_b_conf_stack = std_b_math_confidence
replace std_b_conf_stack = std_b_physics_confidence if subject == "physics"

gen std_b_anx_stack = std_b_math_anxiety
replace std_b_anx_stack = std_b_physics_anxiety if subject == "physics"



gen b_anx_miss = .
replace b_anx_miss = b_physics_anxiety_miss if subject == "physics"
replace b_anx_miss = b_math_anxiety_miss if subject == "math"

gen b_conf_miss = .
replace b_conf_miss = b_physics_confidence_miss if subject == "physics"
replace b_conf_miss = b_math_confidence_miss if subject == "math"



* rescaling the fac_female variable
replace fac_female = fac_female*10


gen femXfac_female = female_r*fac_female


eststo t31: `qui' reghdfe std_e_anx_stack female_r fac_female femXfac_female age_imp reservation_stu_imp father_college_imp ///
	mother_college_imp fac_age_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r ///
	fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r ///
	std_b_anx_stack b_anx_miss, a(department_id subject) vce(clus stdid)


eststo t32: `qui' reghdfe std_e_conf_stack female_r fac_female femXfac_female age_imp reservation_stu_imp father_college_imp ///
	mother_college_imp fac_age_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r ///
	fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r ///
	std_b_conf_stack b_conf_miss , a(department_id subject) vce(clus stdid)


esttab t31 t32, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ci(fmt(%9.3f) par) p(fmt(%9.2f)) ) starlevels(* 0.1 ** 0.05 *** 0.01)


* ===================================================================== *
* -----------------     	 Heterogeneity            ----------------- *
* ===================================================================== *


use `full', clear 


eststo t21: `qui' reghdfe std_coursegrade femXfac_fem if low_acadskills==1 , a(stdid classfe) vce(clus facid)

eststo t22: `qui' reghdfe std_coursegrade femXfac_fem if low_acadskills==0 , a(stdid classfe) vce(clus facid)


eststo t23: `qui' reghdfe std_coursegrade femXfac_fem if low_belonging==1 , a(stdid classfe) vce(clus facid)

eststo t24: `qui' reghdfe std_coursegrade femXfac_fem if low_belonging==0 , a(stdid classfe) vce(clus facid)


eststo t25: `qui' reghdfe std_coursegrade femXfac_fem if low_confidence==1 , a(stdid classfe) vce(clus facid)

eststo t26: `qui' reghdfe std_coursegrade femXfac_fem if low_confidence==0 , a(stdid classfe) vce(clus facid)


eststo t27: `qui' reghdfe std_coursegrade femXfac_fem if low_anxiety==1 , a(classfe stdid) vce(clus facid)

eststo t28: `qui' reghdfe std_coursegrade femXfac_fem if low_anxiety==0 , a(classfe stdid) vce(clus facid)



esttab t2*, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ci(fmt(%9.3f) par) p(fmt(%9.2f)) ) starlevels(* 0.1 ** 0.05 *** 0.01)


* ===================================================================== *
* -----------------   Heterogeneity (std. scores)     ----------------- *
* ===================================================================== *

use `stdscores', clear 


eststo s121: `qui' reghdfe std_g3 female_r fac_female femXfac_female age_imp reservation_stu_imp father_college_imp mother_college_imp fac_age_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r std_g1 g1_miss age_miss female_miss reservation_stu_miss father_college_miss mother_college_miss if low_acadskills==1  , a(department_id sub) vce(clus stdid)

eststo s122: `qui' reghdfe std_g3 female_r fac_female femXfac_female age_imp reservation_stu_imp father_college_imp mother_college_imp fac_age_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r std_g1 g1_miss age_miss female_miss reservation_stu_miss father_college_miss mother_college_miss if low_acadskills==0  , a(department_id sub) vce(clus stdid)


eststo s123:  reghdfe std_g3 female_r fac_female femXfac_female age_imp reservation_stu_imp father_college_imp mother_college_imp fac_age_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r std_g1 g1_miss age_miss female_miss reservation_stu_miss father_college_miss mother_college_miss if low_belonging==1  , a(department_id sub) vce(clus stdid)

eststo s124:  reghdfe std_g3 female_r fac_female femXfac_female age_imp reservation_stu_imp father_college_imp mother_college_imp fac_age_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r std_g1 g1_miss age_miss female_miss reservation_stu_miss father_college_miss mother_college_miss if low_belonging==0  , a(department_id sub) vce(clus stdid)


eststo s125: `qui' reghdfe std_g3 female_r fac_female femXfac_female age_imp reservation_stu_imp father_college_imp mother_college_imp fac_age_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r std_g1 g1_miss age_miss female_miss reservation_stu_miss father_college_miss mother_college_miss if low_confidence==1  , a(department_id sub) vce(clus stdid)

eststo s126: `qui' reghdfe std_g3 female_r fac_female femXfac_female age_imp reservation_stu_imp father_college_imp mother_college_imp fac_age_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r std_g1 g1_miss age_miss female_miss reservation_stu_miss father_college_miss mother_college_miss if low_confidence==0  , a(department_id sub) vce(clus stdid)


eststo s127: `qui' reghdfe std_g3 female_r fac_female femXfac_female age_imp reservation_stu_imp father_college_imp mother_college_imp fac_age_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r std_g1 g1_miss age_miss female_miss reservation_stu_miss father_college_miss mother_college_miss if low_anxiety==1  , a(department_id sub) vce(clus stdid)

eststo s128: `qui' reghdfe std_g3 female_r fac_female femXfac_female age_imp reservation_stu_imp father_college_imp mother_college_imp fac_age_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r std_g1 g1_miss age_miss female_miss reservation_stu_miss father_college_miss mother_college_miss if low_anxiety==0  , a(department_id sub) vce(clus stdid)



esttab s12*, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ci(fmt(%9.3f) par) p(fmt(%9.2f)) ) starlevels(* 0.1 ** 0.05 *** 0.01)




* Tweak this to control the Low/High vertical spacing within each row
local off 0.12

coefplot ///
    (t23, keep(femXfac_fem) rename(femXfac_fem="Effect on course grades (SD)")    ///
          offset( `off') msymbol(o) mcolor(gs3) mfcolor(gs3) ciopts(lcolor(gs3))) ///
        (t24, keep(femXfac_fem) rename(femXfac_fem="Effect on course grades (SD)")    ///
          offset(-`off') msymbol(o) mcolor(gs10) mfcolor(gs10)  ciopts(lcolor(gs10))) ///
    (s123, keep(femXfac_female) rename(femXfac_female="Effect on test scores (SD)")  ///
          offset(`off') msymbol(o) mcolor(gs3) mfcolor(gs3)  ciopts(lcolor(gs3))) ///
    (s124, keep(femXfac_female) rename(femXfac_female="Effect on test scores (SD)")  ///
          offset(-`off') msymbol(o) mcolor(gs10) mfcolor(gs10) ciopts(lcolor(gs10))), ///
    legend(order( 2 "Lower belonging " 4 "Higher belonging") rows(1) ring(1) pos(6)) ///
    xline(0, lpattern(shortdash) lcolor(red)) ///
    xscale(range(-0.15 0.50)) xlabel(-0.15(0.1)0.50,  angle(0)) ///
    order("Course grades" "Test scores") ///
    ylabel(, angle(0) ) xtitle("")

graph export "`output_dir'/fig_fxbelong.pdf", as(pdf) replace

* ===================================================================== *
* -----------------     	 Student beliefs          ----------------- *
* ===================================================================== *

use `full', clear 


collapse *_r *_miss *_imp fac_female, by(stdid subject department_id)

* keep only math and physics observations for students 
keep if inlist(subject, "math", "physics")


gen you_equityminded = you_equityminded_math_r
replace you_equityminded = you_equityminded_physics_r if subject == "physics"


* rescaling the fac_female variable
replace fac_female = fac_female*10
gen femXfac_female = female_r*fac_female


eststo t41: `qui' reghdfe you_equityminded femXfac_female female_r fac_female  age_imp reservation_stu_imp father_college_imp ///
	mother_college_imp fac_age_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r fac_phd_r ///
	fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r ///
	age_miss female_miss reservation_stu_miss father_college_miss mother_college_miss  ///
	, a(department_id subject) vce(clus stdid)

lincom femXfac_female + fac_female 

estadd scalar efull = `r(estimate)'
estadd scalar sefull = `r(se)' 
estadd scalar ciufull  = `r(ub)' 
estadd scalar cilfull  = `r(lb)' 
estadd scalar pfull = `r(p)'

summ you_equityminded if female_r == 0
estadd scalar Mmean = `r(mean)'

summ you_equityminded if female_r == 1
estadd scalar Fmean = `r(mean)'



esttab t41, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ci(fmt(%9.3f) par) p(fmt(%9.2f)) ) starlevels(* 0.1 ** 0.05 *** 0.01) ///
	scalar(Fmean Mmean efull sefull cilfull ciufull pfull)  sfmt(%9.3f %9.3f %9.3f %9.3f %9.2f)


* ===================================================================== *
* -----------------    Student demographics           ----------------- *
* ===================================================================== *

use `full', clear 

preserve 

capture confirm variable b_received_tutoring_r
if _rc {
	capture confirm variable b_received_tutoring
	if !_rc {
		capture confirm numeric variable b_received_tutoring
		if !_rc {
			gen b_received_tutoring_r = b_received_tutoring
		}
		else {
			gen b_received_tutoring_r = b_received_tutoring
			replace b_received_tutoring_r = "" if b_received_tutoring_r == "NA"
			destring b_received_tutoring_r, replace
		}
	}
}

gen b_ask_ques = b_ask_questions2_r ==1
replace b_ask_ques = . if b_ask_questions2_r ==.

gen b_call = (b_mathstudy_25_r + b_physicsstudy_25_r)/2
gen b_discuss = (b_mathstudy_27_r + b_physicsstudy_27_r)/2
gen b_office = (b_mathstudy_29_r + b_physicsstudy_29_r)/2

summ b_salary_expected_r
gen std_b_salary_expected = (b_salary_expected_r - `r(mean)')/`r(sd)'

summ b_n_friends_r
gen std_b_n_friends = (b_n_friends_r - `r(mean)')/`r(sd)'

loc chars "std_b_acadskills std_b_anxiety std_b_confidence father_college_r mother_college_r reservation_stu_r b_belonging b_ask_ques b_received_tutoring_r b_call b_discuss b_office std_b_salary_expected std_b_n_friends you_equityminded_math_r you_equityminded_physics_r"

collapse `chars' female_r, by(stdid department_id)

loc i = 1
foreach char in `chars' {

	eststo s1`i': `qui' reg `char' female_r, robust

	`qui' summ `char' if female_r ==1
	loc femmean = `r(mean)'

	`qui' summ `char' if female_r ==0
	loc malemean = `r(mean)'

	estadd scalar Fmean = `femmean'
	estadd scalar Mmean = `malemean'


	loc i = `i' + 1
	
} 


esttab s1*, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ci(fmt(%9.3f) par) p(fmt(%9.2f)) ) starlevels(* 0.1 ** 0.05 *** 0.01) scalar(Fmean Mmean)

restore



* ===================================================================== *
* -----------------      Baseline balance             ----------------- *
* ===================================================================== *


loc chars "std_b_acadskills std_b_jee std_b_anxiety std_b_confidence  father_college_r mother_college_r reservation_stu_r female_r b_belonging"
loc i = 1

foreach char in `chars' {

	eststo s2`i': `qui' reghdfe `char' fac_female, a(coursefe) vce(clus facid)
	loc i = `i' + 1
	
} 


esttab s2?, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ci(fmt(%9.3f) par) p(fmt(%9.2f)) ) starlevels(* 0.1 ** 0.05 *** 0.01)




eststo s2F: `qui' reghdfe fac_female `chars' , a(coursefe) vce(clus facid)
estadd scalar pF = Ftail(e(df_m), e(df_r), e(F))



esttab s2F, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ci(fmt(%9.3f) par) p(fmt(%9.2f)) ) starlevels(* 0.1 ** 0.05 *** 0.01) scalar(F pF) 



* ===================================================================== *
* -----------------    Faculty characteristics        ----------------- *
* ===================================================================== *

use `full', clear 

 


loc chars "fac_associate_professor_r fac_professor_r fac_yearsinhighed_r fac_phd_r fac_phdprog_master fac_degree_college_elite_r reservation_fac_r "


collapse `chars' fac_female, by(facid)

loc i = 1
foreach char in `chars' {

	eststo s2b`i': `qui' reg `char' fac_female, robust


	`qui' summ `char' if fac_female ==1
	loc femmean = `r(mean)'

	`qui' summ `char' if fac_female ==0
	loc malemean = `r(mean)'

	estadd scalar Fmean = `femmean'
	estadd scalar Mmean = `malemean'


	loc i = `i' + 1
	
} 


esttab s2b?, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ci(fmt(%9.3f) par) p(fmt(%9.2f)) ) starlevels(* 0.1 ** 0.05 *** 0.01) scalar(Fmean Mmean)  sfmt(%9.3f %9.3f)


* ===================================================================== *
* -----------------    Effect on males students       ----------------- *
* ===================================================================== *

use `full', clear


eststo s321: `qui' reghdfe std_coursegrade femXfac_fem fac_female, a(stdid coursefe) vce(clus facid)
lincom femXfac_fem + fac_female 
estadd scalar efull = `r(estimate)'
estadd scalar sefull = `r(se)' 
estadd scalar ciufull  = `r(ub)' 
estadd scalar cilfull  = `r(lb)' 
estadd scalar pfull = `r(p)'

eststo s322: `qui' reghdfe std_coursegrade femXfac_fem fac_female ///
 fac_age_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r ///
 fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r  ///
, a(stdid coursefe) vce(clus facid)

lincom femXfac_fem + fac_female 
estadd scalar efull = `r(estimate)'
estadd scalar sefull = `r(se)' 
estadd scalar ciufull  = `r(ub)' 
estadd scalar cilfull  = `r(lb)' 
estadd scalar pfull = `r(p)'


esttab s32*, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ci(fmt(%9.3f) par) p(fmt(%9.2f)) ) starlevels(* 0.1 ** 0.05 *** 0.01) ///
	scalar(efull sefull cilfull ciufull pfull)  sfmt(%9.3f %9.3f %9.3f %9.3f %9.2f)


* ===================================================================== *
* -----------------     	    Robustness            ----------------- *
* ===================================================================== *

use `full', clear


* equally weighted classrooms
eststo s311: `qui' reghdfe std_coursegrade femXfac_fem if class_size>2 [pw = class_weight], a(stdid coursefe classfe) vce(clus facid)


* non-elite only
eststo s312: `qui' reghdfe std_coursegrade femXfac_fem if elite==0, a(stdid coursefe classfe) vce(clus facid)

* non-standardized
eststo s313: `qui' reghdfe coursegrade femXfac_fem, a(stdid coursefe classfe) vce(clus facid)

* restricted to STEM courses
eststo s314: `qui' reghdfe std_coursegrade femXfac_fem if notstem==0, a(stdid coursefe classfe) vce(clus facid)



esttab s31*, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ci(fmt(%9.3f) par) p(fmt(%9.2f)) ) starlevels(* 0.1 ** 0.05 *** 0.01)


* ===================================================================== *
* -----------------    Belonging and engagement       ----------------- *
* ===================================================================== *

use `full', clear 


collapse *_r *_miss *_imp fac_female, by(stdid department_id)

* rescaling the fac_female variable
replace fac_female = fac_female*10
gen femXfac_female = female_r*fac_female


gen e_ask_ques = e_ask_questions2_r ==1
replace e_ask_ques = . if e_ask_questions2_r ==.

gen b_ask_ques = b_ask_questions2_r ==1
replace b_ask_ques = . if b_ask_questions2_r ==.

eststo s41: `qui' reghdfe e_stay_branch_r female_r fac_female femXfac_female age_imp reservation_stu_imp father_college_imp ///
	mother_college_imp fac_age_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r ///
	fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r b_stay_branch_imp  ///
	age_miss female_miss reservation_stu_miss father_college_miss mother_college_miss b_stay_branch_miss,  ///
	a(department_id) vce(robust)

`qui' summ e_stay_branch_r
estadd scalar Dmean = `r(mean)'


eststo s42: `qui' reghdfe e_ask_ques female_r fac_female femXfac_female age_imp reservation_stu_imp father_college_imp ///
	mother_college_imp fac_associate_professor_r fac_age_r fac_professor_r fac_yearsinhighed_r ///
	fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r b_ask_questions2_imp ///
	age_miss female_miss reservation_stu_miss father_college_miss mother_college_miss b_ask_questions2_miss,  ///
	a(department_id) vce(robust)

`qui' summ e_ask_ques
estadd scalar Dmean = `r(mean)'


esttab s4?, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ci(fmt(%9.3f) par) p(fmt(%9.2f)) ) starlevels(* 0.1 ** 0.05 *** 0.01) scalar(Dmean) 



* ===================================================================== *
* ----------      Faculty interactions with students  ----------------- *
* ===================================================================== *

use `full', clear


collapse *_r *_imp *_miss low_anxiety low_belonging low_acadskills low_confidence fac_female, by(stdid subject department_id elite)


* keep only math and physics observations for students 
keep if inlist(subject, "math", "physics")

* call on student
gen b_call = b_mathstudy_25_imp
replace b_call = b_physicsstudy_25_imp if subject == "physics"

gen e_call = e_mathstudy_25_r
replace e_call = e_physicsstudy_25_r if subject == "physics"

gen b_call_miss = b_mathstudy_25_miss
replace b_call_miss = b_physicsstudy_25_miss if subject == "physics"

* discuss during breaks
gen b_discuss = b_mathstudy_27_imp
replace b_discuss = b_physicsstudy_27_imp if subject == "physics"

gen e_discuss = e_mathstudy_27_r
replace e_discuss = e_physicsstudy_27_r if subject == "physics"

gen b_discuss_miss = b_mathstudy_27_miss
replace b_discuss_miss = b_physicsstudy_27_miss if subject == "physics"


* office hour
gen b_office = b_mathstudy_29_imp
replace b_office = b_physicsstudy_29_imp if subject == "physics"

gen e_office = e_mathstudy_29_r
replace e_office = e_physicsstudy_29_r if subject == "physics"

gen b_office_miss = b_mathstudy_29_miss
replace b_office_miss = b_physicsstudy_29_miss if subject == "physics"


* rescaling the fac_female variable
replace fac_female = fac_female*10
gen femXfac_female = female_r*fac_female


eststo s71: `qui' reghdfe e_call female_r fac_female femXfac_female age_imp reservation_stu_imp father_college_imp ///
	mother_college_imp fac_age_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r ///
	fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r ///
	age_miss female_miss reservation_stu_miss father_college_miss mother_college_miss b_ask_questions2_miss  ///
	b_call b_call_miss, a(department_id sub) vce(clus stdid)

`qui' summ e_call
estadd scalar Dmean = `r(mean)'	


eststo s72: `qui' reghdfe e_discuss female_r fac_female femXfac_female age_imp reservation_stu_imp father_college_imp ///
	mother_college_imp fac_age_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r ///
	fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r ///
	age_miss female_miss reservation_stu_miss father_college_miss mother_college_miss b_ask_questions2_miss  ///
	b_discuss b_discuss_miss, a(department_id sub) vce(clus stdid)

`qui' summ e_discuss
estadd scalar Dmean = `r(mean)'	

eststo s73: `qui' reghdfe e_office female_r fac_female femXfac_female age_imp reservation_stu_imp father_college_imp ///
	mother_college_imp fac_age_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r ///
	fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r ///
	age_miss female_miss reservation_stu_miss father_college_miss mother_college_miss b_ask_questions2_miss  ///
	b_office b_office_miss, a(department_id sub) vce(clus stdid)

`qui' summ e_office
estadd scalar Dmean = `r(mean)'	


esttab s71 s72 s73, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ci(fmt(%9.3f) par) p(fmt(%9.2f)) ) starlevels(* 0.1 ** 0.05 *** 0.01)  scalar(Dmean)


eststo s74: `qui' reghdfe e_call female_r fac_female femXfac_female age_imp reservation_stu_imp father_college_imp ///
	mother_college_imp fac_age_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r ///
	fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r ///
	age_miss female_miss reservation_stu_miss father_college_miss mother_college_miss b_ask_questions2_miss  ///
	b_call b_call_miss if low_acadskills==1, a(department_id sub) vce(clus stdid)

`qui' summ e_call if low_acadskills==1
estadd scalar Dmean = `r(mean)'	


eststo s75: `qui' reghdfe e_discuss female_r fac_female femXfac_female age_imp reservation_stu_imp father_college_imp ///
	mother_college_imp fac_age_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r ///
	fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r ///
	age_miss female_miss reservation_stu_miss father_college_miss mother_college_miss b_ask_questions2_miss  ///
	b_discuss b_discuss_miss if low_acadskills==1, a(department_id sub) vce(clus stdid)

`qui' summ e_discuss if low_acadskills==1
estadd scalar Dmean = `r(mean)'	

eststo s76: `qui' reghdfe e_office female_r fac_female femXfac_female age_imp reservation_stu_imp father_college_imp ///
	mother_college_imp fac_age_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r ///
	fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r ///
	age_miss female_miss reservation_stu_miss father_college_miss mother_college_miss b_ask_questions2_miss  ///
	b_office b_office_miss if low_acadskills==1, a(department_id sub) vce(clus stdid)

`qui' summ e_office if low_acadskills==1
estadd scalar Dmean = `r(mean)'	


esttab s74 s75 s76, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ci(fmt(%9.3f) par) p(fmt(%9.2f)) ) starlevels(* 0.1 ** 0.05 *** 0.01)  scalar(Dmean)


* ===================================================================== *
* -----------------     Teacher practices             ----------------- *
* ===================================================================== *

use `full', clear


collapse *_r tpi_* fac_female, by(department_id facid)


`qui' ds wh_*_r
loc i = 1
foreach w in `r(varlist)' {
	eststo s61`i': `qui' reghdfe `w' fac_female fac_associate_professor_r fac_professor_r fac_yearsinhighed_r ///
		fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r  , a(department_id) vce(robust)

	summ `w'
	estadd scalar Dmean = `r(mean)'
	
	loc i = `i' + 1
} 

esttab s61*, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ci(fmt(%9.3f) par) p(fmt(%9.2f)) ) starlevels(* 0.1 ** 0.05 *** 0.01) scalar(Dmean)


loc i = 1
foreach t in inclass assignment feedback collab {
	eststo s62`i': `qui' reghdfe tpi_`t' fac_female fac_associate_professor_r fac_professor_r fac_yearsinhighed_r ///
		fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r , a(department_id) vce(robust)

	summ tpi_`t'
	estadd scalar Dmean = `r(mean)'	

	loc i = `i' + 1	
}

esttab s62*, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ci(fmt(%9.3f) par) p(fmt(%9.2f)) ) starlevels(* 0.1 ** 0.05 *** 0.01) scalar(Dmean)


* ===================================================================== *
* -----------------        Faculty beliefs            ----------------- *
* ===================================================================== *

use `full', clear 

preserve

* estimating means of outcomes

gen stereo_maj = better_response  == 4 | better_response  == 5
gen stereo_ms = better_response_ms   == 4 | better_response_ms   == 5
gen stereo_st = better_response_st   == 4 | better_response_st   == 5

collapse stereo_* fac_female, by(facid)


foreach s in stereo_maj stereo_ms stereo_st {
	`qui' summ `s' if fac_female == 1
	loc Sf`s' = `r(mean)'

	`qui' summ `s' if fac_female == 0
	loc Sm`s' = `r(mean)'

}


restore


eststo s51: `qui' reghdfe std_coursegrade femXfemaleeq_st femXfemalest_st, a(stdid classfe) vce(clus facid)

estadd scalar Fmean = `Sfstereo_st'
estadd scalar Mmean = `Smstereo_st'

eststo s52: `qui' reghdfe std_coursegrade femXfemaleeq_ms femXfemalest_ms, a(stdid classfe) vce(clus facid)

estadd scalar Fmean = `Sfstereo_ms'
estadd scalar Mmean = `Smstereo_ms'

eststo s53: `qui' reghdfe std_coursegrade femXfemaleeq femXfemalest, a(stdid classfe) vce(clus facid)

estadd scalar Fmean = `Sfstereo_maj'
estadd scalar Mmean = `Smstereo_maj'

esttab s51 s52 s53, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ci(fmt(%9.3f) par) p(fmt(%9.2f)) ) starlevels(* 0.1 ** 0.05 *** 0.01) scalar(Fmean Mmean)



* ===================================================================== *
* -----------------        Faculty mindset            ----------------- *
* ===================================================================== *


preserve

* estimating means of outcomes

collapse fixed_mindset fixed_mindsetl fac_female, by(facid)

foreach s in fixed_mindset fixed_mindsetl {
	`qui' summ `s' if fac_female == 1
	loc Sf`s' = `r(mean)'

	`qui' summ `s' if fac_female == 0
	loc Sm`s' = `r(mean)'

}

restore


eststo s54: `qui' reghdfe std_coursegrade femXfem_gro femXfem_fix , a(stdid classfe) vce(clus facid)

estadd scalar Fmean = `Sffixed_mindset'
estadd scalar Mmean = `Smfixed_mindset'


eststo s55: `qui' reghdfe std_coursegrade femXfem_grol femXfem_fixl, a(stdid classfe) vce(clus facid)

estadd scalar Fmean = `Sffixed_mindsetl'
estadd scalar Mmean = `Smfixed_mindsetl'


esttab s54 s55, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ci(fmt(%9.3f) par) p(fmt(%9.2f)) ) starlevels(* 0.1 ** 0.05 *** 0.01)  scalar(Fmean Mmean)



* ===================================================================== *
* -----------------    Faculty qualifications         ----------------- *
* ===================================================================== *


preserve

* estimating means of outcomes

collapse fac_phd_r assoabv fac_female, by(facid)

foreach s in fac_phd_r assoabv {
	`qui' summ `s' if fac_female == 1
	loc Sf`s' = `r(mean)'

	`qui' summ `s' if fac_female == 0
	loc Sm`s' = `r(mean)'

}

restore


eststo s54: `qui' reghdfe std_coursegrade femXfem_phdl femXfem_nophdl , a(stdid classfe) vce(clus facid)

estadd scalar Fmean = `Sffac_phd_r '
estadd scalar Mmean = `Smfac_phd_r '


eststo s55: `qui' reghdfe std_coursegrade femXfem_assl femXfem_noassl, a(stdid classfe) vce(clus facid)

estadd scalar Fmean = `Sfassoabv'
estadd scalar Mmean = `Smassoabv'


esttab s54 s55, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ci(fmt(%9.3f) par) p(fmt(%9.2f)) ) starlevels(* 0.1 ** 0.05 *** 0.01)  scalar(Fmean Mmean)



* ===================================================================== *
* -----------------         Mediation analysis        ----------------- *
* ===================================================================== *

use `stdscores', clear 

gen math = subject == "math"

mediate (std_g3 female_r age_r reservation_stu_r father_college_r mother_college_r fac_associate_professor_r fac_professor_r ///
	fac_yearsinhighed_r fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r std_g1) (std_e_anxiety female_r ///
	 age_r reservation_stu_r father_college_r mother_college_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r fac_phd_r ///
	  fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r std_b_anxiety) (fac_female , continuous(0 1))  if female_r , vce(clus stdid) ateterms


mediate (std_g3 female_r age_r reservation_stu_r father_college_r mother_college_r fac_associate_professor_r fac_professor_r ///
	fac_yearsinhighed_r fac_phd_r fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r std_g1) (std_e_confidence female_r ///
	age_r reservation_stu_r father_college_r mother_college_r fac_associate_professor_r fac_professor_r fac_yearsinhighed_r fac_phd_r ///
	fac_phd_in_prog_r reservation_fac_r fac_degree_college_elite_r std_b_confidence) (fac_female , continuous(0 1))  if female_r , vce(clus stdid) ateterms



* ===================================================================== *
* -----------------                   	  IV          ----------------- *
* ===================================================================== *

use `full', clear

bys stdid: gen n_sec = _N
bys stdid: egen n_female = total(fac_female)

gen exp_female_LOSO = (n_female - fac_female)/(n_sec - 1) if n_sec > 1


*------------------------------------------------------------
* 1) Domain-specific peer belief items (1 = more stereotypical)
*------------------------------------------------------------
gen bel_math_stereo = 1 - you_equityminded_math_r
gen bel_phys_stereo = 1 - you_equityminded_physics_r


*------------------------------------------------------------
* 2) Section-level peer aggregates (male peers only)
*    (SAME classes for both subjects)
*------------------------------------------------------------
bys classfe: egen Wsec_math = mean(cond(female_r==0, bel_math_stereo, .))
bys classfe: egen Wsec_phys = mean(cond(female_r==0, bel_phys_stereo, .))

bys classfe: egen n_male_bel_m = total(female_r==0 & !missing(bel_math_stereo))
bys classfe: egen n_male_bel_p = total(female_r==0 & !missing(bel_phys_stereo))

* Male peers’ LOSO female-instructor exposure (subject-neutral)
bys classfe: egen Zsec_all = mean(cond(female_r==0, exp_female_LOSO, .))
bys classfe: egen n_male_z  = total(female_r==0 & !missing(exp_female_LOSO))


*------------------------------------------------------------
* 3) Keep focal women; aggregate to student-level W_math, W_phys, and Z_i
*    using precision weights implicitly via counts
*------------------------------------------------------------
keep if female_r == 1

gen wnum_m = Wsec_math * n_male_bel_m
gen wnum_p = Wsec_phys * n_male_bel_p
gen znum   = Zsec_all  * n_male_z

bys stdid: egen W_math_num = total(wnum_m)
bys stdid: egen W_phys_num = total(wnum_p)
bys stdid: egen Z_i_num    = total(znum)

bys stdid: egen den_m = total(n_male_bel_m)
bys stdid: egen den_p = total(n_male_bel_p)
bys stdid: egen den_z = total(n_male_z)

gen W_math = W_math_num/den_m if den_m > 0
gen W_phys = W_phys_num/den_p if den_p > 0
gen Z_i    = Z_i_num   /den_z if den_z > 0


*------------------------------------------------------------
* 4) Student-level outcomes/baselines (endline & baseline)
*------------------------------------------------------------
bys stdid: egen y_math  = mean(e_math_anxiety_r)
bys stdid: egen y_phys  = mean(e_physics_anxiety_r)
bys stdid: egen b_math  = mean(b_math_anxiety_r)
bys stdid: egen b_phys  = mean(b_physics_anxiety_r)

bys stdid: egen y_math2 = mean(e_math_confidence_r)
bys stdid: egen y_phys2 = mean(e_physics_confidence_r)
bys stdid: egen b_math2 = mean(b_math_confidence_r)
bys stdid: egen b_phys2 = mean(b_physics_confidence_r)


*------------------------------------------------------------
* 5) Reduce to one row per student
*------------------------------------------------------------
egen tag_std = tag(stdid)
keep if tag_std


*------------------------------------------------------------
* 6) Standardize outcomes/baselines on unique students
*------------------------------------------------------------
egen y_math_sd  = std(y_math)
egen y_phys_sd  = std(y_phys)
egen b_math_sd  = std(b_math)
egen b_phys_sd  = std(b_phys)

egen y_math2_sd = std(y_math2)
egen y_phys2_sd = std(y_phys2)
egen b_math2_sd = std(b_math2)
egen b_phys2_sd = std(b_phys2)


*------------------------------------------------------------
* 7) Build stacked two-row panel WITHOUT reshape
*------------------------------------------------------------
tempfile base math phys
save `base', replace

* “math” rows
preserve
    use `base', clear
    gen sub2 = "math"

    gen y_   = y_math_sd
    gen b_   = b_math_sd
    gen y_2  = y_math2_sd
    gen b_2  = b_math2_sd
    gen W_   = W_math

    keep stdid sub2 y_ b_ y_2 b_2 W_ Z_i
    save `math', replace
restore

* “physics” rows
preserve
    use `base', clear
    gen sub2 = "physics"

    gen y_   = y_phys_sd
    gen b_   = b_phys_sd
    gen y_2  = y_phys2_sd
    gen b_2  = b_phys2_sd
    gen W_   = W_phys

    keep stdid sub2 y_ b_ y_2 b_2 W_ Z_i
    save `phys', replace
restore

* Stack: 2 rows per student
use `math', clear
append using `phys'


*------------------------------------------------------------
* 8) Domain contrast + interacted instrument
*------------------------------------------------------------
gen d = cond(sub2=="math", 1, -1)
gen Z_int = Z_i * d


*------------------------------------------------------------
* 9) Standardize W within subject explicitly + pp versions
*------------------------------------------------------------
bys sub2: egen W_std = std(W_)

gen W_pp     = 100 * W_
gen Z_int_pp = 100 * Z_int

label var W_pp     "Male peers' stereotype (pp)"
label var Z_int_pp "Instr.: peers' female-exposure × subject (pp)"

label var W_std "Male peers' stereotype (z, within subject)"
label var y_    "Anxiety (z, within subject)"
label var y_2   "Confidence (z, within subject)"
label var b_    "Baseline anxiety (z)"
label var b_2   "Baseline confidence (z)"


*------------------------------------------------------------
* 10) 2SLS with student FE + subject FE; cluster at student
*------------------------------------------------------------

ivreghdfe y_  (W_pp = Z_int_pp)  b_,  absorb(stdid sub2) cluster(stdid) first rf endog(W_pp)

ivreghdfe y_2 (W_pp = Z_int_pp)  b_2, absorb(stdid sub2) cluster(stdid) first rf endog(W_pp)

log close
exit, clear
