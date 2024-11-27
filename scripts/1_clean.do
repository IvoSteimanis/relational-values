*--------------------------------------------------
* 1_0_clean.do
* Authors: Max Burger & Ivo Steimanis
* Philipps University Marburg
*--------------------------------------------------


*--------------------------------------------------
* Description
*--------------------------------------------------
/*
 1) Load raw dataset
 2) Cleaning: labels, renaming of variables
 3) Generating new variables
*/
*--------------------------------------------------


*-----------------------
* 1) Load raw dataset
*-----------------------
import excel "$working_ANALYSIS\data\Honiara_2022_raw", sheet("Honiara_2022") clear

replace FZ="Welcoming Attitude [1 - not at all ,5 - very]" in 1 
drop C E G BR EQ IR
rename ID ID2
rename IF IF2
rename IN IN2


foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

sort A

drop in 1/13 // Functionality tests
destring *, replace


gen country=1


*--------------------------------------------
* 2) Cleaning: labels, renaming of variables
*--------------------------------------------
*** Rename to lower case
rename *, lower


*** Rename variables
** Setup
rename a start
rename b end

label define interviewer1 1	"Simba" 2	"Matheus" 3	"Dwayne" 4	"Jarren" 5	"Edlyn" 6 "Marry" 7 "Tracey" 
encode d, generate(interviewer) label(interviewer1)

rename f date_ra

rename h interview_village
replace interview_village = "Henderson Island Settlement" if interview_village == "Reef Island Settlement" // Other then planned and initially communicated by the Reef Island Settlement (RIS) chief, we were not allowed to collect data there. Instead, we went to Henderson Island Settlement (HIS). However, we did not change the name in the survey but RIS was chosen when actually people went to (HIS).

rename i notes

label define yes_no1 0 "No" 1 "Yes"
encode k, gen(particip_2017) label(yes_no1)

encode l, gen(islander) label(yes_no1)

rename m treat
label define treat 0 "Control" 1 "Treatment" , replace
label values treat treat
 
rename p surname
rename q name
rename r familyname



*--------------------------------------------
* Module A: Socio-Econ 1
*--------------------------------------------
label define female1 0 "Male" 1 "Female", replace
encode s, generate(female) label(female1)
lab var female "Female"

label define marital1 1 "Never married" 2 "Married" 3 "Widowed" 4 "Divorced" 5 "Abandoned / separated" 6 "Other", replace
encode t, generate(marital) label(marital1)

rename u children

label define hh_decision1 1	"Me" 2 "My spouse" 3 "Me and my spouse" 4 "Someone else" , replace
encode v , gen(hh_decision) label(hh_decision1)
rename w hh_decision_other

rename x age

rename y edu_yr

label define edu_highest1 1 "No school" 2 "Primary" 3 "Form 3" 4 "Form 5" 5 "Form 6" 6 "Form 7" 
encode z , gen(edu_highest) lab(edu_highest1)

rename ab college
rename ac bachelor
rename ad masters
rename ae doctoral
rename af none

rename ah living_province
rename ai living_village

encode aj, gen(living_here_always) label(yes_no1)
rename ak living_here_years

encode al, gen(home) label(yes_no1)

rename an home_province
rename ao home_village

lab define ethni 1 "Melanesian" 2 "Polynesian" 3 "Micronesian" 4 "Other"
encode ap , gen(ethnicity) lab(ethni)
rename aq ethnicity_other

rename as pidgin
rename at english
rename au melanesian
rename av polynesian
rename ax language_other

gen traditional = 0
replace traditional = 1 if language_other == "Mother tongue" | language_other == "Traditional language" | language_other == "Pidgin and mother tongue" | language_other == "Santa Cruz language" | language_other == "Waneagu" | language_other == "Kara dialect" | language_other == "Kula" | language_other == "Kwara'ae"  
lab var traditional "Mother tongue / Traditional language used day-to-day basis"
gen lhs_language = 0
replace lhs_language = 1 if strpos(language_other, "Lord") > 0  
lab var lhs_language  "LHS-Language used day-to-day basis"

foreach var in pidgin english melanesian polynesian  {
lab val `var' yes_no1
}


*--------------------------------------------
* Module B: Climate Change beliefs
*--------------------------------------------
label define yes_no_dk1 0 "No" 1 "Yes" 99 "Don't know"
encode az , gen(cc_belief1) label(yes_no_dk1)
encode ba , gen(cc_belief2) label(yes_no_dk1)
encode bb , gen(cc_belief3) label(yes_no_dk1)

rename bd cc_uncertain
rename be cc_agency

rename bi fut_droughts_often
rename bj fut_droughts_longer
rename bk fut_cyclones_often
rename bl fut_rain_often
rename bm fut_floods_stronger
rename bn fut_intrusion_stroner
rename bo fut_sea_higher
rename bp fut_erosion_more


*--------------------------------------------
* Module C: Relational Value Scenario
*--------------------------------------------
gen ckl = .
replace ck = "" if co != . // if stated that would like to give more than 200
destring ck, replace

replace cl = "" if co != . // if stated that would like to give more than 200
destring cl, replace

replace ckl = ck if ck != .
replace ckl = cl if cl != .
replace ckl = co if co != .
rename ckl wtp

rename cn time_scenario

rename cp wtp_reason
rename cq wtp_reason_no

rename cs allocation_percent

rename cv rv3_1
rename cw rv3_2
rename cx rv3_3
rename cy rv3_4

rename cz rv4
rename da rv5

encode dd , gen(rv6_most) 
encode de , gen(rv6_second)
encode df , gen(rv6_third)
encode dg , gen(rv6_least)
lab define reason_protect 1 "Instrumental: Provides clean air and water" 2 "Intrinsic: Itself/Regardless of use to us" 3 "Relational (Culture): Connected to our culture" 4 "Relational (Stewardship): Feel responsibel for it"
foreach var in rv6_*  {
	lab val `var' reason_protect
}

rename di rv7_1
rename dj rv7_2
rename dk rv7_3
rename dl rv7_4



*--------------------------------------------
* Module D: Ladders
*--------------------------------------------
** Life Satisfaction
rename dn ladder_life_now
lab var ladder_life_now "Current position on life satisfaction ladder"
rename do ladder_life_future
lab var ladder_life_future "Position on life satisfaction ladder in 5 years"
rename  dp ladder_life_aspiration
lab var ladder_life_now "Highest achievable position on life satisfaction ladder"
rename  dq ladder_life_home
lab var ladder_life_now "Position when still living at home-place"




** Economic Ladder
rename dt ladder_econ_now
lab var ladder_econ_now "Current position on economic ladder"
rename du ladder_econ_years
lab var ladder_econ_years "Years on current step of economic ladder"

rename dy ladder_econ_before
lab var ladder_econ_before "Prior position on economic ladder"
rename dz ladder_econ_before_years
lab var ladder_econ_before_years "Year on prior step of economic ladder"
rename ec ladder_econ_max
lab var ladder_econ_max "Highest step on ladder within last 10 years"
rename ed ladder_econ_min
lab var ladder_econ_min "Lowest step on ladder within last 10 years"
rename ef ladder_econ_future
lab var ladder_econ_future "Position on economic ladder in 5 years"
rename eg  ladder_econ_aspiration
lab var ladder_econ_aspiration "Highest achievable position on economic ladder"

** Agency
rename ej econ_aspiration
rename ek econ_knowledge
rename el econ_agency1
rename em econ_agency2
rename en econ_agency3
rename eo econ_agency7
rename ep econ_agency8




*--------------------------------------------
* Module E: Environmental Hazards
*--------------------------------------------

** Information on Hazard
rename es hazard_number

*** Floods 2014
encode eu, gen(hazard_affected) label(yes_no1)
encode ev, gen(hazard_place_same) label(yes_no1)
rename ex hazard_place_province
rename ey hazard_place_village

** Injuries / Destruction by hazard
* Persons Injured
rename fa hazard_injured_self
lab var hazard_injured_self "Person got injured"
rename fb hazard_injured_other
lab var hazard_injured_other "Other household member got injured"
rename fc hazard_killed_other
lab var hazard_killed_other "Other household members got killed"
rename fd hazard_injured_none
lab var hazard_injured_none "Nobody was injured or killed"

* House damaged
lab define house_damage1 0 "No, it was not damaged" 1 "Yes, partially damaged" 2 "Yes, totally damaged" 99 "I did not own a house at that time"
encode fe, gen(hazard_house_damage) lab (house_damage1)
lab var hazard_house_damage "House damaged by hazard"

* Land lost
lab define land_lost1 0 "No, I did lose no land" 1 "Yes, I lost land" 99 "I did not own land at that time"
encode ff , gen(hazard_land_lost) lab(land_lost1)
lab var hazard_land_lost "Land lost due to hazard"

* Other buildings damaged
lab define buildings_damage1 0 "No, it was not damaged" 1 "Yes, partially damaged" 2 "Yes, totally damaged"  99 "I did not own other buildings at that time"
encode fg, gen(hazard_buildings_damage) lab(buildings_damage1)
lab var hazard_buildings_damage "Other buildings damaged by hazard"

* Animals harmed / killed
lab define animals_harmed1 0 "No, none harmed" 1 "Yes, injured" 2 "Yes, killed"  99 "I did not own animals at that time"
encode fh, gen(hazard_animals_harmed) lab(animals_harmed1)
lab var hazard_animals_harmed "Animals harmed by hazard"

* Other assets destroyed
lab define assets_destroyed1 0 "no, undamaged" 1 "Yes, totally destroyed" 2 "Yes, partially damaged" 99 "did not own other assets at that time"
encode fi, gen(hazard_assets_damage) lab(assets_destroyed1)
lab var hazard_assets_damage "Other assets destroyed by hazard"

** Rebuilding Costs
* House
rename fj house_rebuild_sbd
replace house_rebuild_sbd = -1 if house_rebuild_sbd == 1
lab var house_rebuild_sbd "Rebuilding costs (-1 if don't know)"
rename fk house_rebuild_days
lab var house_rebuild_days "Rebuilding time (-1 if don't know)"
encode fl , generate(house_rebuild_cond) lab(worse_better)
label var house_rebuild_cond "Condition of house afterwards"




** Movement after Hazard
lab define hazard_move1 0 "Stayed" 1 "Moved away"
encode fm , gen(hazard_move) lab(hazard_move1)
lab var hazard_move "Moved away permanently after hazard"

lab define hazard_stay_wanted 0	"Had no other option" 1	"Wanted to stay" 
encode fn, gen(hazard_stay_delib) lab(hazard_stay_wanted)
lab var hazard_stay_delib "Stayed after hazard because wanted to"

lab define hazard_move_where1 0 "Moved to another place" 1 "Moved to the place living at today"
encode fo , gen(hazard_move_where) lab(hazard_move_where1)
lab var hazard_move_where "Moved to the place living at today after hazard"

rename fq hazard_move_province
rename fr hazard_move_village

label define worse_better1 -1 "worse" 0 "same" 1 "better"
encode ft, gen(hazard_move_network) label(worse_better)
encode fu, gen(hazard_move_economic) label(worse_better)
encode fv, gen(hazard_move_natural) label(worse_better)
encode fw, gen(hazard_move_exposure) label(worse_better)
encode fx, gen(hazard_move_living) label(worse_better)
encode fy, gen(hazard_move_connection) label(worse_better)

rename fz hazard_move_reception
lab var hazard_move_reception "Reception at arrival in destination (1 - very unwelcoming ; 5 very welcoming)"

rename ga rebuild_total_here
lab var rebuild_total_here "In total, how often did you rebuild your house in current place?"


** Should help
rename gc gov_should_help
rename gd ngo_should_help
rename ge rich_should_help
rename gf relig_should_help
rename gg comm_should_help
rename gh bank_should_help
rename gi insurer_should_help
rename gj family_should_help
rename gk friends_should_help
rename gl neighbors_should_help
rename gm noone_should_help
lab var gov_should_help "Government should help deal with CCC"
lab var ngo_should_help "NGOs should help deal with CCC"
lab var rich_should_help "Rich countries causing CC should help deal with CCC"
lab var relig_should_help "Religious organization should help deal with CCC"
lab var comm_should_help "Local community should help deal with CCC"
lab var bank_should_help "Bank should help deal with CCC"
lab var insurer_should_help "My insurance provider should help deal with CCC"
lab var family_should_help "Family should help deal with CCC"
lab var friends_should_help "Friends should help deal with CCC"
lab var neighbors_should_help "Neighbors should help deal with CCC"
lab var noone_should_help "No one should help deal with CCC"

** Would help
rename go gov_would_help
rename gp ngo_would_help
rename gq rich_would_help
rename gr relig_would_help
rename gs comm_would_help
rename gt bank_would_help
rename gu insurer_would_help // needs to be encoded
rename gv family_would_help
rename gw friends_would_help
rename gx neighbors_would_help

lab var gov_would_help "Government would help deal with CCC"
lab var ngo_would_help "NGOs would help deal with CCC"
lab var rich_would_help "Rich countries causing CC would help deal with CCC"
lab var relig_would_help "Religious organization would help deal with CCC"
lab var comm_would_help "Local community would help deal with CCC"
lab var bank_would_help "Bank would help deal with CCC"
lab var insurer_would_help "My insurance provider would help deal with CCC"
lab var family_would_help "Family would help deal with CCC"
lab var friends_would_help "Friends would help deal with CCC"
lab var neighbors_would_help "Neighbors would help deal with CCC"


** Adaptation
rename ha cc_adapt_house
replace cc_adapt_house = 0 if cc_adapt_house ==.
lab var cc_adapt_house "Reinforced the house"
rename hb cc_adapt_store
replace cc_adapt_store = 0 if cc_adapt_store ==.
lab var cc_adapt_store "Store belongings on elevated level"
rename hc cc_adapt_stilts
replace cc_adapt_stilts = 0 if cc_adapt_stilts ==.
lab var cc_adapt_stilts "Rebuild on stilts"
rename hd cc_adapt_fortify
replace cc_adapt_fortify = 0 if cc_adapt_fortify ==.
lab var cc_adapt_fortify "Fortified the land"
rename he cc_adapt_other
replace cc_adapt_other = 0 if cc_adapt_other == .
lab var cc_adapt_other "Other measure taken"
rename hf cc_adapt_other_which
lab var cc_adapt_other_which "Type of other measures taken"

gen cc_adapt = .
replace cc_adapt = 0 if cc_adapt_house == 0 & cc_adapt_store == 0 & cc_adapt_stilts == 0 & cc_adapt_fortify == 0 & cc_adapt_other == 0
replace cc_adapt = 1 if cc_adapt_house == 1 | cc_adapt_store == 1 | cc_adapt_stilts == 1 | cc_adapt_fortify == 1 | cc_adapt_other == 1
lab var cc_adapt "Any adaptation measures taken"
lab val cc_adapt yes_no

rename hh cc_noadapt_fortified
lab var cc_noadapt_fortified "House/Land already well protected"
rename hi cc_noadapt_hazard
lab var cc_noadapt_hazard "Not necessary as environmental hazards won't be severe"
rename hj cc_noadapt_resources
lab var cc_noadapt_resources "No resources available"
rename hk cc_noadapt_know
lab var cc_noadapt_know  "Don't know how to protect"
rename hl cc_noadapt_move1
lab var cc_noadapt_move1 "Will move away when it gets to bad"
rename hm cc_noadapt_move2 
lab var cc_noadapt_move2 "Will move away anyways"
rename hn cc_noadapt_dk
lab var cc_noadapt_dk "Never thought about it / Don't know"
rename ho cc_noadapt_unable
lab var cc_noadapt_unable "Nothing I could do to protect my house and land"
rename hp cc_noadapt_other
lab var cc_noadapt_other "Other reason"

foreach var in cc_adapt_house cc_adapt_store cc_adapt_stilts cc_adapt_fortify cc_adapt_other cc_noadapt_fortified cc_noadapt_hazard cc_noadapt_resources cc_noadapt_know cc_noadapt_move1 cc_noadapt_move2 cc_noadapt_dk cc_noadapt_unable {
lab val `var' yes_no1
}


** Adaptation Suggestion
rename hs cc_suggest_wall 
rename ht cc_suggest_trees
rename hu cc_suggest_stones
rename hv cc_suggest_migrate
rename hw cc_suggest_inland
rename hx cc_suggest_other
rename hy cc_suggest_dk
rename hz cc_suggest_other_which





*--------------------------------------------
* Module F: Migration
*--------------------------------------------
lab define lifestyle1 0	"Having a secure job and earning money in Honiara." 1 "Live the island life and do some fishing and gardening."
encode ib , gen (lifestyle) lab(lifestyle1)
lab var lifestyle "Prefer island-life"

lab define move_pref1 0 "Settlement" 1 "Atoll" 
encode ic , gen(move_pref) lab(move_pref1)

rename id2 stay_honiara

lab define move_ability1 1 "Extremely difficult" 2 "Moderately difficult" 3 "Slightly difficult" 4 "Neither easy nor difficult" 5 "Slightly easy" 6 "Moderately easy" 7 "Extremely easy"
encode ie , gen(move_ability) label(move_ability1)

encode if2 , gen (move_ability_friends) label(yes_no1)
encode ig , gen (move_ability_means) label(yes_no1)
encode ih , gen (move_ability_job) label(yes_no1)


*--------------------------------------------
* Module G: Norms
*--------------------------------------------
encode ii, gen(norm_believe) 
encode ij, gen(norm_never_visit) label(yes_no1)
rename ik norm_visit_freq
encode il, gen(norm_language) label(yes_no1)
encode im, gen(norm_lifestyle) label(yes_no1)
lab define accepable 1 "Absolutely not acceptable" 2 "Not really acceptable" 3 "Acceptable"  4 "Absolutely acceptable"
encode in2, gen(norm_chief) label(accepable)
encode io, gen(norm_marry) label(accepable)
replace norm_chief = . if norm_chief == 5
replace norm_marry = . if norm_marry == 5
rename ip norm_visit_home
rename iq norm_receive_visit


*--------------------------------------------
* Module H: Personality
*--------------------------------------------
rename iu opti1
rename iv opti2
rename iw opti3
rename ix opti4
rename iy opti5
rename iz opti6
rename ja opti7
rename jb opti8
rename jc opti9
rename jd opti10
rename je enjoy_community
rename jf network_support

rename ji place1
rename jj place2
rename jk place3
rename jl place4
rename jm place5
rename jn place6
rename jo place7
rename jp place8
rename jq place9
rename jr place10
rename js place11
rename jt place12

encode jv, generate(children_education) label(worse_better)
encode jw, generate(children_economic) label(worse_better)
encode jx, generate(children_satisfac) label(worse_better)
encode jy, generate(children_community) label(worse_better)
encode jz, generate(children_environ) label(worse_better)

rename kc time
rename ke risk

rename kg trust_general
rename kh trust_community
rename ki trust_family

rename kk pos_recip
rename kl neg_recip
rename km altruism

label define frequency 4 "Every day" 3 "Once a week" 2 "Once a month" 1 "Couple times a year" 0 "Never"
encode kn, gen(community_work) label(frequency)

label define likelihood 5 "Very likely" 2 "Somewhat likely" 3 "Neither likely nor unlikely" 2 "Somewhat unlikely" 1 "Very unlikely"
encode ko, gen(community_work_sanction) label(likelihood)




*--------------------------------------------
* Module I: Socio-economics II
*--------------------------------------------
rename kp hh_member
rename kq hh_member_18

rename kr income

rename ks hh_income_average
winsor2 hh_income_average, cuts(0 95)
lab var hh_income_average "Income HH average"
rename kt hh_income_good 
label var hh_income_good "Income HH good month"
rename ku hh_income_bad 
lab var hh_income_bad "Income HH bad month"

// rename mx remmitance_average

** Eaten too little
lab define low_nutri1 4 "almost every day" 3 "almost every week" 2	"almost every month" 1 "some months but not every month" 0 "never"
encode kv, gen(low_nutri) lab(low_nutri1)




** Spendings
rename kw budget_sparetime
rename kx budget_temptation
rename ky budget_saving

** Immovables Assets
rename la agri_land1
rename lb non_agri_land1
rename lc house1
rename ld other_building1

** Movable Assets
foreach v of varlist lf lg li lo ls lu lv lw {
   local x : variable label `v'
   rename `v' `x'1
}
rename *, lower
rename lh phone1
rename lj solar1
rename lk fridge1
rename ll sound_system1
rename lm stove1
rename ln sewing_machine1
rename lp fiberglass_boat1
rename lq wood_boat1
rename lr engine1
rename lt motor_cycle1

foreach var in agri_land non_agri_land house other_building radio television phone generator solar fridge sound_system stove sewing_machine chainsaw fiberglass_boat wood_boat engine car motor_cycle bicycle plough watertank {
encode `var'1 , gen(`var') label(yes_no1)
}



** Materials house is build of
rename ly house_stone
rename lz house_brick
rename ma house_mud_wood
rename mb house_mud_cement
rename mc house_wood
rename md house_iron
rename me house_grass_straw
rename mf house_tin

label var house_stone "Stone used to build house"
label var house_brick "Brick used to build house"
label var house_mud_wood "Mud and wood used to build house"
label var house_mud_cement "Mud and cement used to build house"
label var house_wood "Wood used to build house"
label var house_iron "Iron used to build house"
label var house_grass_straw "Grass / Straw used to build house"
label var house_tin "Tin used to build house"


** Health
rename mg health


** Problems in area
rename mi prob_food1
rename mj prob_sanitation1
rename mk prob_disease1
rename ml prob_crime1
rename mm prob_hazard1
rename mn prob_pollution1 
rename mo prob_poverty1
rename mp prob_population1
rename mq prob_jobs1
rename mr prob_social_service1 
rename ms prob_credit1
rename mt prob_transfer1
rename mu prob_housing1
rename mv prob_tranpsort1
rename mw prob_work1
rename mx prob_conflict1
rename my prob_land1
rename mz prob_corruption1
rename na prob_gender_ineq1


foreach var in prob_food prob_sanitation prob_disease prob_crime prob_hazard prob_pollution prob_poverty prob_population prob_jobs prob_social_service prob_credit prob_transfer prob_housing prob_tranpsort prob_work prob_conflict prob_land prob_corruption prob_gender_ineq {
encode `var'1, gen(`var') label(yes_no1)
}





** Participate in Associations
encode nb, generate(member_assosi) lab(yes_no)

rename nd member_neighborhood
rename ne member_district
rename nf member_migrant
rename ng member_livelihood
rename nh member_farmer
rename ni member_formal_political
rename nj member_informal_political
rename nk member_student
rename nl member_women
rename nm member_cultural
rename nn member_sport
rename np member_other
label var member_neighborhood "Member in Neighbourhood association"
label var member_district "Member in Local district association"
label var member_migrant "Member in Migrant association"
label var member_livelihood "Member in Cooperative associated with your livelihood"
label var member_farmer "Member in Farmers association"
label var member_formal_political "Member in Formal political association"
label var member_informal_political "Member in Informal political association"
label var member_student "Member in Student association"
label var member_women "Member in Women's association"
label var member_cultural "Member in Cultural association"
label var member_sport "Member in Sport association"
label var member_other "Member in other"

rename ns comments
lab var comments "Comments"

rename nt phone_nr
lab var phone_nr "Phone number of participant"





*--------------------------------------------
* Adjusting variables
*--------------------------------------------
// Remove leading and trailing blanks of string variables
ds, has(type string)
foreach var of varlist `r(varlist)' {
    replace `var' = strtrim(`var')
}


// Province living
* Guadalcanal Province (Honiara)
replace living_province = "Honiara" if living_province == "Honi"
replace living_province = "Guadalcanal" if living_province == "Lordhow"
replace living_province = "Guadalcanal" if living_province == "Lordhowe"



// Province Home: Provinces named as home written "inconsistently"; therefore, homogenization necessary
* Malaita Province
replace home_province = "Malaita" if home_province == "Malaita outer island"
replace home_province = "Malaita" if home_province == "Malaita outter"
replace home_province = "Malaita" if home_province == "Malaita outter islands"
replace home_province = "Malaita" if home_province == "Malaita province"
replace home_province = "Malaita" if home_province == "MALAITA"
replace home_province = "Malaita" if home_province == "Mailata"

* Central Province
replace home_province = "Central" if home_province == "Central islands"

* Choiseul Province
replace home_province = "Choiseul" if home_province == "Choisuel"

* Guadalcanal Province (Honiara)
replace home_province = "Guadalcanal" if home_province == "Guadalcanal "
replace home_province = "Guadalcanal" if home_province == "Lordhowe"
replace home_province = "Guadalcanal" if home_province == "Lordhowe"

* Makira and Ulawa Province
replace home_province = "Makira and Ulawa" if home_province == "Making ulawa"
replace home_province = "Makira and Ulawa" if home_province == "Makira"
replace home_province = "Makira and Ulawa" if home_province == "Makira Ulawa"
replace home_province = "Makira and Ulawa" if home_province == "Ulawa"

* Western Province
replace home_province = "Western" if home_province == "Moi"
replace home_province = "Western" if home_province == "Moi"
replace home_province = "Western" if home_province == "NuSA"
replace home_province = "Western" if home_province == "Ranoga"
replace home_province = "Western Province" if home_province == "Rendova"
replace home_province = "Western" if home_province == "Roviana"
replace home_province = "Western" if home_province == "Western"
replace home_province = "Western" if home_province == "Western"
replace home_province = "Western" if home_province == "Western province"
replace home_province = "Western" if home_province == "Western Province"

* Rennel and Bellona (RenBel)
replace home_province = "Rennel and Bellona" if home_province == "Renbel"
replace home_province = "Rennel and Bellona" if home_province == "Rennel"
replace home_province = "Rennel and Bellona" if home_province == "Rental & Belona"

* Isabel Province
replace home_province = "Isabel" if home_province == "Sasamunga"

* Temotu Province
replace home_province = "Temotu" if home_province == "Temotu"
replace home_province = "Temotu" if home_province == "Temotu"












*------------------------
* 3) Generating
*-------------------------
// Drop practicing observations
drop if date == "2021-11-25" | date == "2022-06-06" // Enumerators practicing

// Keep only observations from main survey (not pre-test)
drop if date == "2022-06-01"

// Drop observation with missing values in key-characteristics
drop if age == .

*unique identifier
gen id=_n

// Participants referring to other place then main island (Guadalcanal) as home
gen outside_hon = 0
replace outside_hon = 1 if living_province != "Guadalcanal" & living_province != "Honiara" // Living currently in province other than Honiara/guadalcanal
replace outside_hon = 1 if home_province != "Guadalcanal" & home_province != ""            // Living currently in Honiara, but considering other province home
lab var outside_hon "Guadalcanal not considered home"
lab define outside_hon1 0 "Guadalcanal (Honiara)" 1 "Other Province", replace
lab val outside_hon outside_hon1

// Single
gen single = 0
replace single = 1 if marital == 8
lab var single "Single"

// log income
gen ln_income = ln(income+1)
lab var ln_income "Log of income+1"

gen ln_hh_income_average = ln(hh_income_average+1)
lab var ln_hh_income_average "Log of average hh-income income+1"


// Time Sum Survey
rename n time_start
rename nq time_end
gen time_sum= round((time_end - time_start) * 60 * 24)
lab var time_sum "Total time with participan (min)"


// Time Sum Scenario
rename bq time_start_scenario
rename cm time_end_scenario
gen time_sum_scenario = round((time_end_scenario - time_start_scenario) * 60 * 24)

lab var time_scenario "Time in scenario (sec)"
* scatter time_scenario time_sum_scenario // just to check

// Date of survey
split start, p("T" "." "+") gen(start_)
drop start_2 start_3 start_4
rename start_1 date 
lab var date "Date of interview"

// PCA: TIES 
** Generate necessary variables
* Variable for speaking any "traditional language"
gen poly_mela_trad = 0
replace poly_mela_trad = 1 if melanesian == 1 | polynesian == 1 | language_other == "Mother tongue" | language_other == "Traditional language"

pca outside_hon lifestyle poly_mela_trad norm_never_visit norm_language norm_lifestyle 
predict ties_pca 
lab var ties_pca "Ties to island (PCA)"

** Generate medians
* High ties & stay
foreach var in  ties_pca {
egen med_`var' = median(`var')
gen `var'_high = .
replace `var'_high = 0 if `var' <= med_`var'
replace `var'_high = 1 if `var' > med_`var' & med_`var' != .
}
lab var ties_pca_high "Above median ties"



// Place where interview was conducted
gen place = .
replace place = 1 if interview_village == "Lord Howe Settlement"
replace place = 2 if interview_village == "Henderson Island Settlement"
replace place = 3 if interview_village == "Mataniko EA6" | interview_village == "Mataniko EA7"
replace place = 4 if interview_village == "Vura EA13" | interview_village == "Vura EA14" 
lab define place1 1 "LHS" 2 "HIS" 3 "Mat." 4 "Vura"
lab val place place1
lab var place "Place where interview was conducted"


// Winsorized WTP to reduce outliers
gen wtp_wins = wtp
replace wtp_wins = 200 if wtp > 200
lab var wtp_wins  "WTP winsorized at 200"

//PPP adjust wtp_wins:  from World Bank 2022 conversion factor: 6.77
gen wtp_ppp = wtp_wins/6.77


// WTP categories
gen wtp_cat = .
replace wtp_cat = 0 if wtp == 0
replace wtp_cat = 1 if wtp > 0 & wtp < 25
replace wtp_cat = 2 if wtp >= 25 & wtp <= 90
replace wtp_cat = 3 if wtp == 100
replace wtp_cat = 4 if wtp > 100
lab define wtp_cat1 0 "0" 1 "0-24" 2 "25-90" 3 "100" 4 ">100"
lab val wtp_cat wtp_cat1
lab var wtp_cat "WTP categories"

// Log WTP
gen ln_wtp = ln(wtp_wins+1)
lab var ln_wtp "Logarithm of WTP"

// Binary willingness to pay (No/Yes)
gen d_wtp = 0
replace d_wtp = 1 if wtp > 0
lab var d_wtp "Willingness to pay > 0"
gen d_wtp100=d_wtp*100


// Willingness to pay below 60 (60 set as "anchor")
gen wtp_below60 = 1
replace wtp_below60 = 0 if wtp >= 60
lab var wtp_below60 "WTP < SBD60"



// Index Relational Values
alpha rv3*, gen(rel_val)
lab var rel_val "Index: Higher index indicate higher RV"

lab var rv3_1 "RV1"
lab var rv3_2 "RV2"
lab var rv3_3 "RV3"
lab var rv3_4 "RV4"
lab var rel_val "Average index (RV1-5)"
lab var rv4 "Intrinsic"
lab var rv5 "Instrumental"

egen count_rel_val = count(id), by(rel_val)
egen share_wtp_ppp = mean(wtp_ppp), by(rel_val)

*three categories of RV: low, medium-high, maximum
gen rel_cat = 0
replace rel_cat = 1 if rel_val >3 & rel_val <5
replace rel_cat = 2 if rel_val==5
lab def rally 0 "Index:1-3 (n=89)" 1 "Index: 3.25-4.75 (n=409)" 2 "Index: 5 (n=308)", replace
lab val rel_cat rally

// Winsorized governmental budget allocation
gen allocation_percent_wins = allocation_percent
replace allocation_percent_wins = 30 if allocation_percent > 30
lab var allocation_percent_wins "Increase in Budget (winsorized at 30%)"

egen share_allocation_wins = mean(allocation_percent_wins), by(rel_val)
egen count_allo = count(id), by(share_allocation_wins)

// Ranking: Reasons for conservation most important
tab rv6_most, gen(rv6_most)
*ranking of different alternatives
gen instrumental = 1 if rv6_most==1
replace instrumental = 2 if rv6_second==1
replace instrumental = 3 if rv6_third==1
replace instrumental = 4 if rv6_least==1
lab def rel_ranking 1 "Most important" 2 "2nd choice" 3 "3rd choice" 4 "Least important", replace
lab val instrumental rel_ranking
lab var instrumental "Instrumental"

gen intrinsic = 1 if rv6_most==2
replace intrinsic = 2 if rv6_second==2
replace intrinsic = 3 if rv6_third==2
replace intrinsic = 4 if rv6_least==2
lab val intrinsic rel_ranking
lab var intrinsic "Intrinsic"

gen relational_culture = 1 if rv6_most==3
replace relational_culture = 2 if rv6_second==3
replace relational_culture = 3 if rv6_third==3
replace relational_culture = 4 if rv6_least==3
lab val relational_culture rel_ranking
lab var relational_culture "RV: Culture"

gen relational_steward = 1 if rv6_most==4
replace relational_steward = 2 if rv6_second==4
replace relational_steward = 3 if rv6_third==4
replace relational_steward = 4 if rv6_least==4
lab val relational_steward rel_ranking
lab var relational_steward "RV: Steward"
	
gen avg_relational = (relational_culture +  relational_steward)/2
lab var avg_relational "Average rank: RV"

lab def opinion_ranking 1 "Instrumental" 2 "Intrinsinc" 3 "Relational: Culture" 4 "Relational: Stewardship", replace




// Visits
** Should visit more then once per year
gen visit_often = 0
replace visit_often = 1 if norm_visit_freq > 1
lab var visit_often "Should visit more than once a year"

** Number of visits TO home (winsorised ad 3)
gen norm_visit_home_wins = norm_visit_home
replace norm_visit_home_wins = 3 if norm_visit_home > 3 & norm_visit_home != .
lab var norm_visit_home_wins "Visits to place considered home (winsorized at 3)"

** Number of visits FROM home (winsorised ad 3)
gen norm_receive_visit_wins = norm_receive_visit 
replace  norm_receive_visit_wins = 3 if norm_receive_visit > 3 & norm_receive_visit != .
lab var norm_receive_visit_wins "Visitors from place considered home (winsorized at 3)"

** Total numbers of visits (TO & FROM) 
egen visit_receive = rowtotal(norm_visit_home norm_receive_visit), missing
lab var visit_receive "Visits to & from place considered home"

** Total numbers of visits (TO & FROM; winsorised ad 5)
gen visit_receive_wins = visit_receive 
replace visit_receive_wins = 5 if visit_receive > 5 & visit_receive != .
lab var visit_receive_wins "Visits to & from place considered home (winsorized at 5)"


// Ties to island
** Norms regarding home traditional/island way of life
pca norm_never_visit norm_language norm_lifestyle norm_chief norm_marry
predict norm_pca
lab var norm_pca "Norms regarding traditions/ties (higher value, higher agreem. with norms)"

// Education
gen edu = .
replace edu = 1 if edu_highest == 1 | edu_highest == 2 // No schooling (4%) or primary (7%)
replace edu = 2 if edu_highest == 3 | edu_highest == 4 // Form 3 (20%) or Form 5 (27%)
replace edu = 3 if edu_highest == 5 | edu_highest == 6 // Form 6 (27%) / Form 7 (16%)
*replace edu = 4 if college == 1 // 32%
*replace edu = 5 if bachelor == 1 | masters == 1 | doctoral == 1 // bachelor (10%), masters (n=4), doctoral (n=1) 
lab define edu1 1 "Edu: Low (No schooling or primary)" 2 "Edu: Medium (Form 3 or Form 5)" 3 "Edu: High (Form 6 or Form 7)", replace
lab val edu edu1
tab edu, gen(edu)

// Time already living/expecting to live in Honiara
** Time already living as fraction of total years living
gen rel_living_here = living_here_years/age
replace rel_living_here = 0 if living_here_years == 0 & age != .
replace rel_living_here = 1 if living_here_always == 1 & age != .
replace rel_living_here = 1 if rel_living_here > 1 & age != .


// Place attachment (Index)
alpha place1 place2 place3 place4 place5 place6 place7 place8 place9 place10 place11 place12, gen(place_attachment)
lab var place_attachment "Higher values indicate higher attachment to Honiara"


// Wealth (PCA)
foreach var in budget_sparetime budget_temptation budget_saving {
	gen ln_`var' = ln(`var' + 1)
}

pca ln_income ln_hh_income_average low_nutri ln_budget_sparetime ln_budget_temptation ln_budget_saving agri_land non_agri_land house other_building radio television phone generator solar fridge sound_system stove sewing_machine chainsaw fiberglass_boat wood_boat engine car motor_cycle bicycle plough watertank

pca ln_income ln_hh_income_average low_nutri ln_budget_sparetime ln_budget_temptation ln_budget_saving non_agri_land television phone generator fridge sound_system car watertank // dropping all with loading <.2
predict wealth_pca



// Total budget per month
gen budget = budget_sparetime + budget_temptation + budget_saving
lab var budget "Budget for sparetime, temptation, and savings spending"
gen ln_budget = log(budget+1)


	
// Ties to island (Categories)
gen ties_cat = 0                                              // No ties to island 
replace ties_cat = 1 if place == 1                            // Born & living in Lord Howe Settlement
replace ties_cat = 2 if living_here_always == 0  & place == 1 // Moved to Honiara and lives in Lord Howe Settlement
replace ties_cat = 3 if islander == 1 & place == 1            // Guest from atoll this seems only to be correct for LHS

*replace ties_cat=2 for those who moved to honiara from an atoll (reef islands / ontong java)
sort place living_here_always
replace ties_cat = 2 if home_village=="Pileni"
replace ties_cat = 2 if home_village=="Reef Island" 
replace ties_cat = 2 if home_village=="Lipe"
replace ties_cat = 2 if home_village=="Malaku village"
replace ties_cat = 2 if home_village=="Reef"
replace ties_cat = 2 if home_village=="Tuwo"
replace ties_cat = 2 if home_village=="Luaniua"
replace ties_cat = 2 if home_village=="Loaniua"
replace ties_cat = 2 if home_village=="Launiua"
replace ties_cat = 2 if home_village=="Luanua"
replace ties_cat = 2 if home_village=="Pelau"
replace ties_cat = 2 if home_village=="Palau"
replace ties_cat = 2 if home_village=="Malibu"
replace ties_cat = 2 if home_village=="Malibu"
replace ties_cat = 2 if home_village=="Lau lasi island"
replace ties_cat = 2 if home_village=="Lordhowe"
replace ties_cat = 2 if home_village=="Langalanga"
replace ties_cat = 2 if home_village=="Malubu"
replace ties_cat = 2 if home_village=="Malapu"
replace ties_cat = 2 if home_village=="Nifoli"

lab def caty 0 "No atoll connection" 1 "Descendant" 2 "Former inhabitant" 3 "Inhabitant", replace
lab val ties_cat caty
tab ties_cat , gen(ties_cat)

*binary connection identifier
gen atoll_tie=0
replace atoll_tie =1 if  ties_cat>0
lab def ties 0 "No atoll connection" 1 "Atoll connection", replace
lab val atoll_tie ties

// z-scores of DV & IV
foreach var in wtp_wins wtp_cat ln_wtp d_wtp rel_val rv6_most1 rv6_most2 rv6_most3 rv6_most4 ties_cat1 ties_cat2 ties_cat3 ties_cat4 place_attachment age female single ln_income edu2 edu3 treat {
	egen z_`var'= std(`var')
	}	



* Order
*----
order ///
/*setup*/ date date_ra start end time_sum interviewer interview_village notes particip_2017 islander ties_cat ///
/*Module A: socioeconomics I*/ surname name familyname female marital single age edu_yr edu_highest college bachelor masters doctoral none living_province living_village living_here_always living_here_years home home_province home_village outside_hon ethnicity ethnicity_other pidgin english melanesian polynesian traditional lhs_language language_other hh_decision hh_decision_other ///
/*Module B: Climate Change Perception*/ cc_belief1 cc_belief2 cc_belief3 cc_uncertain cc_agency fut_droughts_often fut_droughts_longer fut_cyclones_often fut_rain_often fut_floods_stronger fut_intrusion_stroner fut_sea_higher fut_erosion_more ///
/*Module C: Relational Value Scenario*/ treat time_scenario wtp wtp_reason wtp_reason_no allocation_percent rv3_1 rv3_2 rv3_3 rv3_4 rv4 rv5 rv6_most rv6_second rv6_third rv6_least rv7_1 rv7_2 rv7_3 rv7_4 ///
/*Mocule D: Economic ladder*/ ladder_econ_now ladder_econ_years ladder_econ_before ladder_econ_before_years ladder_econ_max ladder_econ_min ladder_econ_future ladder_econ_aspiration ///
/*Econ Agency*/ econ_aspiration econ_knowledge econ_agency1 econ_agency2 econ_agency3 econ_agency7 econ_agency8 ///
/*Life Ladder*/ ladder_life_now ladder_life_future ladder_life_aspiration ladder_life_home ///
/*Module E: Hazards*/ hazard_number hazard_affected hazard_place_same hazard_place_province hazard_place_village hazard_place_village hazard_injured_self hazard_injured_other hazard_killed_other hazard_injured_none hazard_house_damage hazard_land_lost hazard_buildings_damage hazard_animals_harmed hazard_assets_damage  house_rebuild_sbd house_rebuild_days house_rebuild_cond hazard_move hazard_stay_delib hazard_move_where hazard_move_province hazard_move_village hazard_move_network hazard_move_economic hazard_move_natural hazard_move_exposure hazard_move_living hazard_move_connection hazard_move_reception rebuild_total_here /// 
/*Hazard Expectation and Adaptation*/ gov_should_help ngo_should_help rich_should_help relig_should_help comm_should_help bank_should_help insurer_should_help family_should_help friends_should_help neighbors_should_help noone_should_help gov_would_help ngo_would_help rich_would_help relig_would_help comm_would_help bank_would_help insurer_would_help family_would_help friends_would_help neighbors_would_help cc_adapt cc_adapt_house cc_adapt_store cc_adapt_stilts cc_adapt_fortify cc_adapt_other cc_adapt_other_which cc_noadapt_fortified cc_noadapt_hazard cc_noadapt_resources cc_noadapt_know cc_noadapt_move1 cc_noadapt_move2 cc_noadapt_dk cc_noadapt_unable cc_noadapt_other cc_uncertain cc_agency cc_suggest_wall cc_suggest_trees cc_suggest_stones cc_suggest_migrate cc_suggest_inland cc_suggest_dk cc_suggest_other cc_suggest_other_which ///
/*Module F: Migration*/ lifestyle move_pref stay_honiara move_ability move_ability_friends move_ability_means move_ability_job ///
/*Module G: Norms*/ norm_believe norm_never_visit norm_visit_freq norm_language norm_lifestyle norm_chief norm_marry norm_visit_home norm_receive_visit  ties_pca ties_pca_high  ///
/*Module H: Peresonalitys*/ opti1 opti2 opti3 opti4 opti5 opti6 opti7 opti8 opti9 opti10 enjoy_community network_support place1 place2 place3 place4 place5 place6 place7 place8 place9 place10 place11 place12 time risk trust_general trust_community trust_family pos_recip neg_recip altruism community_work community_work_sanction ///
/*Module I: Socio-Econ II*/ hh_member hh_member_18 children income ln_income hh_income_average ln_hh_income_average hh_income_good hh_income_bad low_nutri budget_sparetime budget_temptation budget_saving  agri_land non_agri_land house other_building radio television phone generator solar fridge sound_system stove sewing_machine chainsaw fiberglass_boat wood_boat engine car motor_cycle bicycle plough watertank house_stone house_brick house_mud_wood house_mud_cement house_wood house_iron house_grass_straw house_tin health ///
/*Module I: SE-Problem location*/ children_education children_economic children_satisfac children_community children_environ prob_food prob_sanitation prob_disease prob_crime prob_hazard prob_pollution prob_poverty prob_population prob_jobs prob_social_service prob_credit prob_transfer prob_housing prob_tranpsort prob_work prob_conflict prob_land prob_corruption prob_gender_ineq ///
/*Module I: SE-Membership*/ member_assosi member_neighborhood member_district member_migrant member_livelihood member_farmer member_formal_political member_informal_political member_student member_women member_cultural member_sport member_other comments phone_nr ///
/*added variables from 2_analysis.do*/ place wtp_wins wtp_ppp wtp_cat ln_wtp d_wtp wtp_below60 allocation_percent_wins rel_val rv6_most1 rv6_most2 rv6_most3 rv6_most4 visit_often norm_visit_home_wins norm_receive_visit_wins visit_receive visit_receive_wins norm_pca edu edu1 edu2 edu3 rel_living_here place_attachment ln_budget_sparetime ln_budget_temptation ln_budget_saving wealth_pca budget ln_budget instrumental intrinsic relational_culture relational_steward avg_relational ties_cat ties_cat1 ties_cat2 ties_cat3 ties_cat4 z_wtp_wins z_wtp_cat z_ln_wtp z_d_wtp z_rv6_most1 z_rv6_most2 z_rv6_most3 z_rv6_most4 z_ties_cat1 z_ties_cat2 z_ties_cat3 z_ties_cat4 z_place_attachment z_age z_female z_single z_ln_income z_edu2 z_edu3 z_treat z_rel_val


keep ///
/*setup*/ country date date_ra start end time_sum interviewer interview_village notes particip_2017 islander ties_cat ///
/*Module A: socioeconomics I*/ surname name familyname female marital single age edu_yr edu_highest college bachelor masters doctoral none living_province living_village living_here_always living_here_years home home_province home_village outside_hon ethnicity ethnicity_other pidgin english melanesian polynesian traditional lhs_language language_other hh_decision hh_decision_other ///
/*Module B: Climate Change Perception*/ cc_belief1 cc_belief2 cc_belief3 cc_uncertain cc_agency fut_droughts_often fut_droughts_longer fut_cyclones_often fut_rain_often fut_floods_stronger fut_intrusion_stroner fut_sea_higher fut_erosion_more ///
/*Module C: Relational Value Scenario*/ treat time_scenario wtp wtp_reason wtp_reason_no allocation_percent rv3_1 rv3_2 rv3_3 rv3_4 rv4 rv5 rv6_most rv6_second rv6_third rv6_least rv7_1 rv7_2 rv7_3 rv7_4 ///
/*Mocule D: Economic ladder*/ ladder_econ_now ladder_econ_years ladder_econ_before ladder_econ_before_years ladder_econ_max ladder_econ_min ladder_econ_future ladder_econ_aspiration ///
/*Econ Agency*/ econ_aspiration econ_knowledge econ_agency1 econ_agency2 econ_agency3 econ_agency7 econ_agency8 ///
/*Life Ladder*/ ladder_life_now ladder_life_future ladder_life_aspiration ladder_life_home ///
/*Module E: Hazards*/ hazard_number hazard_affected hazard_place_same hazard_place_province hazard_place_village hazard_place_village hazard_injured_self hazard_injured_other hazard_killed_other hazard_injured_none hazard_house_damage hazard_land_lost hazard_buildings_damage hazard_animals_harmed hazard_assets_damage  house_rebuild_sbd house_rebuild_days house_rebuild_cond hazard_move hazard_stay_delib hazard_move_where hazard_move_province hazard_move_village hazard_move_network hazard_move_economic hazard_move_natural hazard_move_exposure hazard_move_living hazard_move_connection hazard_move_reception rebuild_total_here /// 
/*Hazard Expectation and Adaptation*/ gov_should_help ngo_should_help rich_should_help relig_should_help comm_should_help bank_should_help insurer_should_help family_should_help friends_should_help neighbors_should_help noone_should_help gov_would_help ngo_would_help rich_would_help relig_would_help comm_would_help bank_would_help insurer_would_help family_would_help friends_would_help neighbors_would_help cc_adapt cc_adapt_house cc_adapt_store cc_adapt_stilts cc_adapt_fortify cc_adapt_other cc_adapt_other_which cc_noadapt_fortified cc_noadapt_hazard cc_noadapt_resources cc_noadapt_know cc_noadapt_move1 cc_noadapt_move2 cc_noadapt_dk cc_noadapt_unable cc_noadapt_other cc_uncertain cc_agency cc_suggest_wall cc_suggest_trees cc_suggest_stones cc_suggest_migrate cc_suggest_inland cc_suggest_dk cc_suggest_other cc_suggest_other_which ///
/*Module F: Migration*/ lifestyle move_pref stay_honiara move_ability move_ability_friends move_ability_means move_ability_job ///
/*Module G: Norms*/ norm_believe norm_never_visit norm_visit_freq norm_language norm_lifestyle norm_chief norm_marry norm_visit_home norm_receive_visit  ties_pca ties_pca_high ///
/*Module H: Personalities*/ opti1 opti2 opti3 opti4 opti5 opti6 opti7 opti8 opti9 opti10 enjoy_community network_support place1 place2 place3 place4 place5 place6 place7 place8 place9 place10 place11 place12 time risk trust_general trust_community trust_family pos_recip neg_recip altruism community_work community_work_sanction ///
/*Module I: Socio-Econ II*/ hh_member hh_member_18 children income ln_income hh_income_average ln_hh_income_average hh_income_good hh_income_bad low_nutri budget_sparetime budget_temptation budget_saving  agri_land non_agri_land house other_building radio television phone generator solar fridge sound_system stove sewing_machine chainsaw fiberglass_boat wood_boat engine car motor_cycle bicycle plough watertank house_stone house_brick house_mud_wood house_mud_cement house_wood house_iron house_grass_straw house_tin health ///
/*Module I: SE-Problem location*/ children_education children_economic children_satisfac children_community children_environ prob_food prob_sanitation prob_disease prob_crime prob_hazard prob_pollution prob_poverty prob_population prob_jobs prob_social_service prob_credit prob_transfer prob_housing prob_tranpsort prob_work prob_conflict prob_land prob_corruption prob_gender_ineq ///
/*Module I: SE-Membership*/ member_assosi member_neighborhood member_district member_migrant member_livelihood member_farmer member_formal_political member_informal_political member_student member_women member_cultural member_sport member_other comments phone_nr ///
/*added variables from 2_analysis.do*/ place wtp_wins wtp_ppp wtp_cat ln_wtp d_wtp wtp_below60 allocation_percent_wins rel_val rv6_most1 rv6_most2 rv6_most3 rv6_most4 visit_often norm_visit_home_wins norm_receive_visit_wins visit_receive visit_receive_wins norm_pca edu edu1 edu2 edu3 rel_living_here place_attachment ln_budget_sparetime ln_budget_temptation ln_budget_saving wealth_pca budget ln_budget instrumental intrinsic relational_culture relational_steward avg_relational ties_cat ties_cat1 ties_cat2 ties_cat3 ties_cat4 z_wtp_wins z_wtp_cat z_ln_wtp z_d_wtp z_rv6_most1 z_rv6_most2 z_rv6_most3 z_rv6_most4 z_ties_cat1 z_ties_cat2 z_ties_cat3 z_ties_cat4 z_place_attachment z_age z_female z_single z_ln_income z_edu2 z_edu3 z_treat z_rel_val d_wtp100 atoll_tie id count_rel_val share_wtp_ppp rel_cat share_allocation_wins count_allo hh_income_average_w




***Save
save "$working_ANALYSIS\processed\si22.dta" , replace





** BANGLADESH
use "$working_ANALYSIS\processed\dhaka_2022_clean1.dta", replace

rename t_scenario treat

gen country=2

replace id=806+id
// Winsorized WTP to reduce outliers
gen wtp_wins = wtp
replace wtp_wins = 800 if wtp > 800
lab var wtp_wins  "WTP winsorized at 800"

//PPP adjust wtp_wins:  from World Bank 2022 conversion factor: 31.36
gen wtp_ppp = wtp_wins/31.36


// Log WTP
gen ln_wtp = ln(wtp_wins+1)
lab var ln_wtp "Logarithm of WTP"

// Binary willingness to pay (No/Yes)
gen d_wtp = 0
replace d_wtp = 1 if wtp > 0
lab var d_wtp "Willingness to pay > 0"
gen d_wtp100=d_wtp*100



// Index Relational Values
rename relational_v1 rv3_1
rename relational_v2 rv3_2
rename relational_v3 rv3_3
rename relational_v4 rv3_4

alpha rv3*, gen(rel_val)
lab var rel_val "Index: Higher index indicate higher RV"

lab var rv3_1 "RV1"
lab var rv3_2 "RV2"
lab var rv3_3 "RV3"
lab var rv3_4 "RV4"
lab var rel_val "Average index (RV1-5)"



egen z_rel_val = std(rel_val)

*three categories of RV: low, medium-high, maximum
gen rel_cat = 0
replace rel_cat = 1 if rel_val >3 & rel_val <5
replace rel_cat = 2 if rel_val==5
lab def rally 0 "Index:1-3" 1 "Index: 3.25-4.75" 2 "Index: 5", replace
lab val rel_cat rally


// Winsorized governmental budget allocation
gen allocation_percent_wins = budget_allocation
replace allocation_percent_wins = 30 if budget_allocation > 30
lab var allocation_percent_wins "Increase in Budget (winsorized at 30%)"



// calculate counts across rel_val categories
egen count_rel_val = count(id), by(rel_val)
egen share_wtp_ppp = mean(wtp_ppp), by(rel_val)
egen share_allocation_wins = mean(allocation_percent_wins), by(rel_val)
egen count_allo = count(id), by(share_allocation_wins)

rename budget_allocation allocation_percent


// Ranking: Reasons for conservation most important
encode v_importance1 , gen(rv6_most) 
encode v_importance2 , gen(rv6_second)
encode v_importance3 , gen(rv6_third)
encode v_importance4 , gen(rv6_least)
lab def opinion_ranking 1 "Instrumental" 2 "Intrinsinc" 3 "Relational: Culture" 4 "Relational: Stewardship", replace
lab val rv6_most opinion_ranking

tab rv6_most, gen(rv6_most)


gen instrumental = 1 if rv6_most==1
replace instrumental = 2 if rv6_second==1
replace instrumental = 3 if rv6_third==1
replace instrumental = 4 if rv6_least==1
lab def rel_ranking 1 "Most important" 2 "2nd choice" 3 "3rd choice" 4 "Least important", replace
lab val instrumental rel_ranking
lab var instrumental "Instrumental"

gen intrinsic = 1 if rv6_most==2
replace intrinsic = 2 if rv6_second==2
replace intrinsic = 3 if rv6_third==2
replace intrinsic = 4 if rv6_least==2
lab val intrinsic rel_ranking
lab var intrinsic "Intrinsic"

gen relational_culture = 1 if rv6_most==3
replace relational_culture = 2 if rv6_second==3
replace relational_culture = 3 if rv6_third==3
replace relational_culture = 4 if rv6_least==3
lab val relational_culture rel_ranking
lab var relational_culture "RV: Culture"

gen relational_steward = 1 if rv6_most==4
replace relational_steward = 2 if rv6_second==4
replace relational_steward = 3 if rv6_third==4
replace relational_steward = 4 if rv6_least==4
lab val relational_steward rel_ranking
lab var relational_steward "RV: Steward"
	
gen avg_relational = (relational_culture +  relational_steward)/2
lab var avg_relational "Average rank: RV"

*socioe-economics
gen single=0
replace single=1 if marital==1

* Wealth (PCA)

pca income income_hh spend_save poor spend_sparetime spend_temptation low_nutri 
predict wealth_pca

keep hh_member id country place treat female marital age edu_yr wtp wtp_reason allocation_percent rv3_1 rv3_2 rv3_3 rv3_4 v_importance1 v_importance2 v_importance3 v_importance4 poor hh_member income income_hh low_nutri spend_sparetime spend_temptation spend_save wtp_wins wtp_ppp ln_wtp d_wtp d_wtp100  rel_val count_rel_val z_rel_val share_wtp_ppp rel_cat allocation_percent_wins share_allocation_wins count_allo rv6_most rv6_second rv6_third rv6_least rv6_most1 rv6_most2 rv6_most3 rv6_most4 instrumental intrinsic relational_culture relational_steward avg_relational single wealth_pca

***Save
save "$working_ANALYSIS\processed\dhaka22.dta" , replace




*Append both datasets
use "$working_ANALYSIS\processed\si22.dta" , replace

append using "$working_ANALYSIS\processed\dhaka22.dta"

lab def countries 1 "Solomon Islands" 2 "Bangladesh", replace
lab val country countries


***Save
save "$working_ANALYSIS\processed\combined.dta" , replace

















