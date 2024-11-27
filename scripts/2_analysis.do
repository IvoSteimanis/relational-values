*--------------------------------------------------
* 02_analysis.do
* Authors: Max Burger & Ivo Steimanis
* Philipps University Marburg
*--------------------------------------------------
use "$working_ANALYSIS\processed\combined.dta" , clear


gen outlier_wtp=0
replace outlier_wtp= 1 if wtp>200 & country==1
replace outlier_wtp=1 if wtp>800 & country==2
bys country: tab outlier_wtp

gen outlier_budget=0
replace outlier_budget=1 if allocation_percent>30
tab outlier_budget outlier_wtp


*differences in outliers between samples and overlap
*WTP
bys countr: tab outlier_wtp
prtest outlier_wtp, by(country)

*Budget
bys countr: tab outlier_budget
prtest outlier_budget, by(country)

*overlap
bys countr: tab outlier_wtp outlier_budget

*-------------------------------------------
*   2.4:  DATA
*------------------------------------------
// Table 1: Summary and balance table (Pooled | Control | Treat)
tab income_hh, gen(inc_cat)
global summary_si atoll_tie female age single edu1 edu2 edu3 hh_member hh_income_average_w wealth_pca   if country==1
global summary_bd female age single edu_yr hh_member inc_cat1 inc_cat2 inc_cat3 inc_cat4 inc_cat5 inc_cat6 inc_cat7 wealth_pca  if country==2


* Balance table
generate t1 = (treat==1)
balancetable (mean if treat==0) (mean if treat==1) (diff t1 if treat!=2) $summary_si using "$working_ANALYSIS/results/tables/Table1_balance_si.xlsx", wide sheet(First sheet) cell(A1) ctitles("Mean" "SD" "Mean" "SD" "Diff" "SE") format(%4.2f) nopar replace  

balancetable (mean if treat==0) (mean if treat==1) (diff t1 if treat!=2) $summary_bd using "$working_ANALYSIS/results/tables/Table1_balance_bd.xlsx", wide sheet(First sheet) cell(A1) ctitles("Mean" "SD" "Mean" "SD" "Diff" "SE") format(%4.2f) nopar replace  

*joint f-test of orthoganility
reg treat $summary_si, robust

reg treat $summary_bd, robust




*-----------------------------------
*   3.1:  Descriptive findings
*-----------------------------------
*Fig 3. Distribution of outcome measures
*Panel A: WTP
mylabels 0(10)50, myscale(@) local(pctlabel) suffix("%") 
twoway (hist wtp_ppp if country==1, bin(20) percent  color("100 143 255%40") lcolor(none) gap(10) xline(9.92, lpattern(dash) lwidth(medthick))) (hist wtp_ppp if country==2, bin(20) percent  color("254 97 0%40") lcolor(none) gap(10) xline(4.41, lwidth(medthick) lpattern(dot))), legend(order(1 "Solomon Islands" 2 "Bangladesh" )) title("{bf: A } WTP ")  xtitle(Amount (PPP adjusted)) xla(0(5)30, nogrid) yla(`pctlabel', nogrid) ytitle("")   xsize(4) ysize(3)  graphregion(color(none))
graph save "$working_ANALYSIS\results\intermediate\figure2a", replace
bys country: sum wtp_ppp,detail
bys country: tab wtp_ppp
ttest wtp_ppp, by(country)

*Panel B: Likert scale
stripplot rel_val, over(country) separate(country) stack  height(.8) width(.05) refline(lpattern(dash) lwidth(medium))   yla(1(1)5, nogrid) yscale(r(0.8 5.2)) xtitle("") msymbol(O) msize(large) mcolor("100 143 255%20" "254 97 0%20" )  ytitle("score") vertical    xla(, noticks) yla(, ang(h))  title("{bf: B } Relational values index") legend(off) graphregion(color(none))
graph save "$working_ANALYSIS\results\intermediate\figure2b", replace
bys country: sum rel_val,detail

ttest rel_val, by(country)


*Panel C: Ranking item
stripplot  avg_relational, over(country) separate(country) stack  height(.8) width(.05) refline(lpattern(dash) lwidth(medium))   yla(1(1)4, nogrid) yscale(r(0.8 4.2)reverse) xtitle("") msymbol(O O) msize(large large) mcolor("100 143 255%20" "254 97 0%20")  ytitle("rank") vertical    xla(, noticks) yla(, ang(h)) title("{bf: C } Relational values rank") legend(off) graphregion(color(none))
graph save "$working_ANALYSIS\results\intermediate\figure2c", replace
bys country: sum avg_relational,detail
ttest avg_relational, by(country)



*Panel D: Government budget allocation

mylabels 0(10)50, myscale(@) local(pctlabel) suffix("%") 
twoway (hist allocation_percent_wins if country==1, bin(30) percent  color("100 143 255%40") lcolor(none) gap(10) xline(9.94, lpattern(dash) lwidth(medthick))) (hist allocation_percent_wins if country==2, bin(30) percent  color("254 97 0%40") lcolor(none) gap(10) xline(6.19, lwidth(medthick) lpattern(dot))), legend(order(1 "Solomon Islands" 2 "Bangladesh" )) title("{bf: D }Government budget allocation")  xtitle(in %-points) xla(0(5)30, nogrid) yla(`pctlabel', nogrid) ytitle("")   xsize(4) ysize(3)  graphregion(color(none)) 
graph save "$working_ANALYSIS\results\intermediate\figure2d", replace
bys country: sum allocation_percent_wins,detail
ttest allocation_percent_wins, by(country)



gr combine "$working_ANALYSIS\results\intermediate\figure2a"  "$working_ANALYSIS\results\intermediate\figure2b" "$working_ANALYSIS\results\intermediate\figure2c" "$working_ANALYSIS\results\intermediate\figure2d", xsize(4) ysize(3) cols(2) scale(1.1) graphregion(margin(0 0 0 0))
graph save "$working_ANALYSIS\results\intermediate\figure3", replace
graph export "$working_ANALYSIS\results\figures\figure3_outcome_distributions.tif", replace width(3650)



*---------------------------
* 3.2 Treatment Effects
*---------------------------
*Fig. 4.	Average treatment effects
global sociodem1 female age single i.edu hh_member hh_income_average_w wealth_pca if country==1
global sociodem2 female age single edu_yr hh_member i.income_hh wealth_pca if country==2

*WTP
eststo wtp1: reg wtp_ppp i.treat if country==1, vce(hc3)
estadd local controls "No"
eststo wtp2: reg wtp_ppp i.treat $sociodem1, vce(hc3)
estadd local controls "Yes"
eststo wtp3: reg wtp_ppp i.treat if country==2, vce(hc3)
estadd local controls "No"
eststo wtp4: reg wtp_ppp i.treat $sociodem2, vce(hc3)
estadd local controls "Yes"

*heterogeneous SI
eststo wtp5: reg wtp_ppp i.treat##i.atoll_tie, vce(hc3)
estadd local controls "No"
eststo wtp6: reg wtp_ppp i.treat##i.atoll_tie $sociodem1, vce(hc3)
estadd local controls "Yes"
margins, dydx(treat) at(atoll_tie=(0 1))
*income effect : For every 500 SBD increase in household income, participants are predicted to to contribute 2.7 SBD more to the protection fund:
margins, dydx() at(wealth_pca=(-5(1)4))
marginsplot


*Panel B: Relational Values Likert index
eststo rv1: reg z_rel_val i.treat  if country==1, vce(hc3)
estadd local controls "No"
eststo rv2: reg z_rel_val i.treat $sociodem1, vce(hc3)
estadd local controls "Yes"
eststo rv3: reg z_rel_val i.treat  if country==2, vce(hc3)
estadd local controls "No"
eststo rv4: reg z_rel_val i.treat $sociodem2, vce(hc3)
estadd local controls "Yes"

*heterogeneous SI
eststo rv5: reg z_rel_val i.treat##i.atoll_tie, vce(hc3)
estadd local controls "No"
eststo rv6: reg z_rel_val i.treat##i.atoll_tie $sociodem1, vce(hc3)
estadd local controls "Yes"
margins, dydx(treat) at(atoll_tie=(0 1))

*Panel C: Average rank relational values (combined culture and steward), lower value implies higer rank in terms of importance of conservation
eststo rank1: reg avg_relational i.treat if country==1, vce(hc3)
estadd local controls "No"
eststo rank2: reg avg_relational i.treat $sociodem1, vce(hc3)
estadd local controls "Yes"
eststo rank3: reg avg_relational i.treat if country==2, vce(hc3)
estadd local controls "No"
eststo rank4: reg avg_relational i.treat $sociodem2, vce(hc3)
estadd local controls "Yes"



*heterogeneous SI
eststo rank5: reg avg_relational i.treat##i.atoll_tie, vce(hc3)
estadd local controls "No"
eststo rank6: reg avg_relational i.treat##i.atoll_tie $sociodem1, vce(hc3)
estadd local controls "Yes"
margins, dydx(treat) at(atoll_tie=(0 1))

*Panel D: Increase in budget spending for envrionment and climate change management
eststo budget1: reg allocation_percent_wins i.treat if country==1, vce(hc3)
estadd local controls "No"
eststo budget2: reg allocation_percent_wins i.treat $sociodem1, vce(hc3)
estadd local controls "Yes"
eststo budget3: reg allocation_percent_wins i.treat if country==2, vce(hc3)
estadd local controls "No"
eststo budget4: reg allocation_percent_wins i.treat $sociodem2, vce(hc3)
estadd local controls "Yes"
margins, at(treat=(0 1))
*heterogeneous SI
eststo budget5: reg allocation_percent_wins i.treat##i.atoll_tie, vce(hc3)
estadd local controls "No"
eststo budget6: reg allocation_percent_wins i.treat##i.atoll_tie $sociodem1, vce(hc3)
estadd local controls "Yes"
margins, dydx(treat) at(atoll_tie=(0 1))


coefplot (wtp2,  label(Solomon Islands)) (wtp4,  label(Bangladesh)),  xsc(r(-40 (10) 40)) bylabel("{bf:A} WTP ($ PPP)") || (rv2, label(Solomon Islands)) (rv4,label(Bangladesh)), xsc(r(-0.5 0.5)) bylabel("{bf:B} RV: Index (in SD)") || (rank2,  label(Solomon Islands)) (rank4, label(Bangladesh)),  xsc(r(-0.5 0.5)) bylabel("{bf:C} RV: Rank") || (budget2, label(Solomon Islands)) (budget4,  label(Bangladesh)),  xsc(r(-5 5)) bylabel("{bf:D} Budget (%-points)") ||,  keep(1.treat) coeflabels(1.treat = "Treated", labsize(6pt))  byopts(compact xrescale rows(1)) xline(0, lpattern(dash) lcolor(gs3)) xtitle("Regression estimated impact relative to control", size(6pt)) grid(none) levels(95 90) ciopts(lwidth(0.8 2)  lcolor(*1 *.3) recast(rcap)) mlabel(cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", "")))) msize(3pt) msymbol(D) mlabsize(10pt) mlabposition(2) subtitle(, size(9pt) lstyle(none) margin(medium) nobox justification(center) alignment(top) bmargin(top)) xsize(4) ysize(2)
gr_edit plotregion1.xaxis1[1].style.editstyle majorstyle(tickstyle(textstyle(size(6-pt)))) editcopy
gr_edit plotregion1.plotregion1[2].style.editstyle margin(medium) editcopy
gr_edit plotregion1.plotregion1[3].style.editstyle margin(large) editcopy
gr_edit legend.plotregion1.label[1].style.editstyle size(7-pt) editcopy
gr_edit legend.plotregion1.label[2].style.editstyle size(7-pt) editcopy
gr_edit style.editstyle margin(medsmall) editcopy
gr save  "$working_ANALYSIS\results\intermediate\figure4_treatment_effects.gph", replace
gr export "$working_ANALYSIS\results\figures\figure4_treatment_effects.tif", replace width(4000)


*Fig. 5.	Heterogeneous treatment effects (Solomon Islands)
coefplot (wtp6), xsc(r(-40 (10) 40)) bylabel("{bf:A} WTP ($ PPP)") || (rv6), xsc(r(-0.5 0.5)) bylabel("{bf:B} RV Index (in SD)") || (rank6),  xsc(r(-0.5 0.5)) bylabel("{bf:C} RV: Rank") || (budget6), xsc(r(-5 5)) bylabel("{bf:D} Budget (%-points)") ||,  keep(1.treat 1.atoll_tie 1.treat#1.atoll_tie) coeflabels(1.treat = "Treated" 1.atoll_tie ="Atoll connection" 1.treat#1.atoll_tie="Treated*Atoll connection" , labsize(6pt))  byopts(compact xrescale rows(1)) xline(0, lpattern(dash) lcolor(gs3)) xtitle("Regression estimated impact relative to control", size(6pt)) grid(none) levels(95 90) ciopts(lwidth(0.8 2)  lcolor(*1 *.3) recast(rcap)) mlabel(cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", "")))) msize(3pt) msymbol(D) mlabsize(10pt) mlabposition(2) subtitle(, size(9pt) lstyle(none) margin(medium) nobox justification(center) alignment(top) bmargin(top))  xsize(5) ysize(2)  
gr_edit plotregion1.xaxis1[1].style.editstyle majorstyle(tickstyle(textstyle(size(6-pt)))) editcopy
gr_edit plotregion1.plotregion1[2].style.editstyle margin(medium) editcopy
gr_edit plotregion1.plotregion1[3].style.editstyle margin(large) editcopy
gr_edit style.editstyle margin(medsmall) editcopy
gr save  "$working_ANALYSIS\results\intermediate\figure5_heterogeneous_SI.gph", replace
gr export "$working_ANALYSIS\results\figures\figure5_heterogeneous_SI.tif", replace width(4000)




*------------------------------------------------------------------------------
* 3.3	Correlation between WTP and budget allocation with relational values  
*------------------------------------------------------------------------------
**Figure 6. Correlation monetary measures with non-monetary measures

*Panel A: raw data correlation
twoway (scatter share_wtp_ppp rel_val [fweight=count_rel_val] if country==1,  mcolor(%5) mlcolor(none)) (lpoly share_wtp_ppp rel_val [fweight=count_rel_val] if country==1,  yla(0(5)15, nogrid) lwidth(medthick) msize(medlarge) lcolor("100 143 255")   bwidth(0.3)) (scatter share_wtp_ppp rel_val [fweight=count_rel_val] if country==2,  mcolor("254 97 0%5") mlcolor(none)) (lpoly share_wtp_ppp rel_val [fweight=count_rel_val] if country==2,  yla(0(5)15, nogrid) lwidth(medthick) msize(medlarge) mcolor("254 97 0")  bwidth(0.3)), legend(order(2 "Solomon Islands" 4 "Bangladesh"))  title("{bf: A:} WTP (raw data)") xtitle("Relational values index") ytitle("$ PPP") graphregion(fcolor(none) lwidth(none))
gr save  "$working_ANALYSIS\results\intermediate\figure6a.gph", replace
pwcorr wtp_ppp rel_val if country==1, sig
pwcorr wtp_ppp rel_val if country==2, sig
pwcorr rel_val wealth_pca if country==1,sig
pwcorr rel_val wealth_pca if country==2,sig

*Panel B: variation at censored values (3 categories) SI
stripplot wtp_ppp if country==1, over(rel_cat) separate(rel_cat) stack  height(0.8) width(.05) refline(lpattern(dash) lwidth(medium)) mcolor(%20 %20 %20)  yla(0(5)30, nogrid) yscale(r(0 30)) xtitle("Solomon Islands (n=806)") msymbol(O) msize(large large large)  ytitle("$ PPP") vertical    xla(, noticks) yla(, ang(h))  title("{bf: B } WTP Variation by RV Index") legend(off) graphregion(color(none))
graph save "$working_ANALYSIS\results\intermediate\figure6b", replace

*Panel C: variation at censored values (3 categories) BD
stripplot wtp_ppp if country==2, over(rel_cat) separate(rel_cat) stack  height(0.8) width(.05) refline(lpattern(dash) lwidth(medium)) mcolor(%20 %20 %20)  yla(0(5)30, nogrid) yscale(r(0 30)) xtitle("Bangladesh (n=302)") msymbol(O) msize(large large large)  ytitle("$ PPP") vertical    xla(, noticks) yla(, ang(h))  title("{bf: C } WTP Variation by RV Index") legend(off) graphregion(color(none))
graph save "$working_ANALYSIS\results\intermediate\figure6c", replace


*Panel D:
pwcorr allocation_percent_wins rel_val if country==1, sig
pwcorr allocation_percent_wins rel_val if country==2, sig
 
twoway (scatter share_allocation_wins rel_val [fweight=count_allo]  if country==1,  mcolor(%5) mlcolor(none)) (lpoly share_allocation_wins rel_val [fweight=count_allo]  if country==1,  yla(0(5)20, nogrid) lwidth(medthick) lcolor("100 143 255") msize(medlarge) mcolor(*.3)  bwidth(0.6)) (scatter share_allocation_wins rel_val [fweight=count_allo]  if country==2,  mcolor("254 97 0%5") mlcolor(none)) (lpoly share_allocation_wins rel_val [fweight=count_allo]  if country==2,  yla(0(5)20, nogrid) lwidth(medthick) msize(medlarge) color("254 97 0") bwidth(0.6)), legend(order(2 "Solomon Islands" 4 "Bangladesh"))  title("{bf: D:} Budget increases (raw data)") xtitle("Relational values index") ytitle("%-points") graphregion(fcolor(none) lwidth(none))
gr save  "$working_ANALYSIS\results\intermediate\figure6d.gph", replace

*Panel E: variation at censored values (3 categories) SI
stripplot allocation_percent_wins if country==1, over(rel_cat) separate(rel_cat) stack  height(0.8) width(.05) refline(lpattern(dash) lwidth(medium)) mcolor(%20 %20 %20)  yla(0(5)30, nogrid) yscale(r(0 30)) xtitle("Solomon Islands (n=806)") msymbol(O) msize(large large large)  ytitle("%-points") vertical    xla(, noticks) yla(, ang(h))  title("{bf: E } Budget Variation by RV Index") legend(off) graphregion(color(none))
graph save "$working_ANALYSIS\results\intermediate\figure6e", replace

*Panel F: variation at censored values (3 categories) BD
stripplot allocation_percent_wins if country==2, over(rel_cat) separate(rel_cat) stack  height(0.8) width(.05) refline(lpattern(dash) lwidth(medium)) mcolor(%20 %20 %20)  yla(0(5)30, nogrid) yscale(r(0 30)) xtitle("Bangladesh (n=302)") msymbol(O) msize(large large large)  ytitle("%-points") vertical    xla(, noticks) yla(, ang(h))  title("{bf: F } Budget Variation by RV Index") legend(off) graphregion(color(none))
graph save "$working_ANALYSIS\results\intermediate\figure6f", replace



gr combine "$working_ANALYSIS\results\intermediate\figure6a"  "$working_ANALYSIS\results\intermediate\figure6b"  "$working_ANALYSIS\results\intermediate\figure6c.gph" "$working_ANALYSIS\results\intermediate\figure6d.gph" "$working_ANALYSIS\results\intermediate\figure6e.gph" "$working_ANALYSIS\results\intermediate\figure6f.gph", xsize(4) ysize(2) cols(3) scale(1.15)
graph save "$working_ANALYSIS\results\intermediate\figure6_correlation_outcomes", replace
graph export "$working_ANALYSIS\results\figures\figure6_correlation_outcomes.tif", replace width(3650)


*regressions
tab rel_cat, gen(rel_cat)
ttest wtp_ppp if rel_cat!=1 & country==1, by(rel_cat)
ttest wtp_ppp if rel_cat!=0 & country==1, by(rel_cat)
ttest allocation_percent_wins if rel_cat!=1 & country==1, by(rel_cat)
ttest allocation_percent_wins if rel_cat!=0 & country==1, by(rel_cat)
ttest allocation_percent_wins if rel_cat!=1 & country==2, by(rel_cat)
ttest allocation_percent_wins if rel_cat!=0 & country==2, by(rel_cat)


eststo wtp_det2: reg wtp_pp rel_cat1 rel_cat3 $sociodem1, vce(robust)

eststo wtp_det1: reg wtp_pp i.rel_cat if country==1, vce(robust)
testparm 1.rel_cat 2.rel_cat, equal
eststo wtp_det2: reg wtp_pp i.rel_cat $sociodem1, vce(robust)
eststo wtp_det3: reg wtp_pp i.rel_cat if country==2, vce(robust)
eststo wtp_det4: reg wtp_pp i.rel_cat $sociodem2, vce(robust)

eststo budget_det1: reg allocation_percent_wins  i.rel_cat  if country==1, vce(robust)
eststo budget_det2: reg allocation_percent_wins  i.rel_cat   $sociodem1, vce(robust)
eststo budget_det3: reg allocation_percent_wins  i.rel_cat  if country==2, vce(robust)
eststo budget_det4: reg allocation_percent_wins  i.rel_cat   $sociodem2, vce(robust)



*----------------------------------------
* Supplementary Online Materials
*----------------------------------------
*Table S1.	Correlation between categorical classification of ties and measures for cultural proximity to people on atoll islands (standardized coefficients)
global ties i.ethnicity pidgin english melanesian polynesian traditional  norm_never_visit norm_language norm_lifestyle lifestyle 
mlogit ties_cat $ties, robust coeflegend

probit atoll_tie treat $ties, vce(robust)
local r2_p= e(r2_p)
eststo ties0: margins, dydx(*) post
estadd scalar r2_p = `r2_p'
mlogit ties_cat treat $ties, vce(robust)
local r2_p= e(r2_p)
eststo ties1: margins, dydx(*) predict(pr outcome(0)) post
estadd scalar r2_p = `r2_p'
mlogit ties_cat treat $ties, vce(robust)
local r2_p= e(r2_p)
eststo ties2: margins, dydx(*) predict(pr outcome(1)) post
estadd scalar r2_p = `r2_p'
mlogit ties_cat treat $ties, vce(robust)
local r2_p= e(r2_p)
eststo ties3: margins, dydx(*) predict(pr outcome(2)) post
estadd scalar r2_p = `r2_p'
mlogit ties_cat treat $ties, vce(robust)
local r2_p= e(r2_p)
eststo ties4: margins, dydx(*) predict(pr outcome(3)) post
estadd scalar r2_p = `r2_p'

*export full regression table
esttab ties0 ties1 ties2 ties3 ties4 using "$working_ANALYSIS\results\Tables\tableSXX_ties_determinants.rtf",keep(treat 2.ethnicity 3.ethnicity pidgin english melanesian polynesian traditional norm_never_visit norm_language norm_lifestyle lifestyle)  transform(ln*: exp(@) exp(@)) mtitles("Any atoll connection" "No atoll connection" "Descendants" "Former inhabitant" "Inhabitant")  b(%4.2f)label stats(N r2_p, labels("N" "Pseudo R2" )  fmt(%4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(,elist(weight:_cons "{break}{hline @width}"))  nonotes addnotes("Notes: Average marginal effects after multinomial Logit regression with robust standard errors. Robust standard errors in brackets: * p<0.05, ** p<0.01, *** p<0.001") replace


*Table S2.	Main effects: Solomon Islands
esttab wtp1 wtp2 rv1 rv2 rank1 rank2 budget1 budget2 using "$working_ANALYSIS\results\tables\tableS2_treatment_SI.rtf", keep(1.treat female age single 2.edu 3.edu hh_member hh_income_average_w wealth_pca _cons) order(1.treat female age single 2.edu 3.edu hh_member hh_income_average_w wealth_pca _cons) label se(%4.2f) transform(ln*: exp(@) exp(@))  mgroups("WTP ($ PPP)" "RV: Index (in SD)" "RV: Rank" "Budget (%-points)", pattern(1 0 1 0 1 0 1 0)) nomtitles b(%4.2f) stats(controls N r2_a , labels("Controls" "N" "Adjusted R-squared") fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(,elist(weight:_cons "{break}{hline @width}")) nonotes  addnotes("Notes: Robust standard errors in brackets: * p < 0.10, ** p < 0.05, *** p < 0.01.") replace


*Table S3.	Main effects: Bangladesh
esttab wtp3 wtp4 rv3 rv4 rank3 rank4 budget3 budget4 using "$working_ANALYSIS\results\tables\tableS3_treatment_BD.rtf", keep(1.treat female age single edu_yr hh_member 2.income_hh 3.income_hh 4.income_hh 5.income_hh 6.income_hh 7.income_hh wealth_pca _cons) order(1.treat female age single edu_yr hh_member 2.income_hh 3.income_hh 4.income_hh 5.income_hh 6.income_hh 7.income_hh wealth_pca _cons) label se(%4.2f) transform(ln*: exp(@) exp(@))  mgroups("WTP ($ PPP)" "RV: Index (in SD)" "RV: Rank" "Budget (%-points)", pattern(1 0 1 0 1 0 1 0)) nomtitles b(%4.2f) stats(controls N r2_a , labels("Controls" "N" "Adjusted R-squared") fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(,elist(weight:_cons "{break}{hline @width}")) nonotes addnotes("Notes: Robust standard errors in brackets: * p < 0.10, ** p < 0.05, *** p < 0.01.") replace



*Table S4.	Solomon Islands: Heterogeneous effects
esttab wtp6 rv6 rank6 budget6 using "$working_ANALYSIS\results\tables\tableS6_heterogeneous_SI.rtf", keep(1.treat 1.atoll_tie 1.treat#1.atoll_tie female age single 2.edu 3.edu hh_member hh_income_average_w wealth_pca _cons) order(1.treat 1.atoll_tie 1.treat#1.atoll_tie female age single 2.edu 3.edu hh_member hh_income_average_w wealth_pca _cons)  label se(%4.2f) transform(ln*: exp(@) exp(@))  mtitles("WTP ($ PPP)" "RV: Index (in SD)" "RV: Rank" "Budget (%-points)") b(%4.2f) stats(controls N r2_a , labels("Controls" "N" "Adjusted R-squared") fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(,elist(weight:_cons "{break}{hline @width}"))nonotes  addnotes("Notes: Robust standard errors in brackets: * p < 0.10, ** p < 0.05, *** p < 0.01.") replace



* Table S5.	Correlation WTP and budget with RV index (SOLOMON ISLANDS)
esttab wtp_det1 wtp_det2 budget_det1 budget_det2  using "$working_ANALYSIS\results\tables\tableS5_corr_RV_WTP_budget_SI.rtf", label se(%4.2f) transform(ln*: exp(@) exp(@))  mgroups("WTP ($ PPP)" "Budget (%-points)", pattern(1 0 1 0)) nomtitles b(%4.2f) stats(controls N r2_a , labels("Controls" "N" "Adjusted R-squared") fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(,elist(weight:_cons "{break}{hline @width}"))nonotes  addnotes("Notes: All estimates are from ordinary least square regressions. Robust standard errors in brackets: * p < 0.10, ** p < 0.05, *** p < 0.01.") replace


* Table S6.	Correlation WTP and budget with RV index (BANGLADESH)
esttab wtp_det3 wtp_det4 budget_det4 budget_det4  using "$working_ANALYSIS\results\tables\tableS6_corr_RV_WTP_budget_BD.rtf", label se(%4.2f) transform(ln*: exp(@) exp(@))  mgroups("WTP ($ PPP)" "Budget (%-points)", pattern(1 0 1 0)) nomtitles b(%4.2f) stats(controls N r2_a , labels("Controls" "N" "Adjusted R-squared") fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(,elist(weight:_cons "{break}{hline @width}"))nonotes  addnotes("Notes: All estimates are from ordinary least square regressions. Robust standard errors in brackets: * p < 0.10, ** p < 0.05, *** p < 0.01.") replace

* Table S7.	Extrapolating WTP to general population in Solomon Islands
tobit wtp_wins i.ethnicity rel_val  $controls, vce(robust) ll(0) ul(200)
margins, at(ethnicity = (1 2 3))
*extrapolations mainly done in excel file called "extrapolation.xlsx"

* Table S8.	Extrapolating budet allocation to general population in Solomon Islands
tobit allocation_percent_wins i.ethnicity rel_val  $controls, vce(robust) ll(1) ul(30)
margins, at(ethnicity = (1 2 3))
*extrapolations mainly done in excel file called "extrapolation.xlsx"




** EOF