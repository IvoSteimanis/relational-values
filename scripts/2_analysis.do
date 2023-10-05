*--------------------------------------------------
* 02_analysis.do
* Authors: Max Burger & Ivo Steimanis
* Philipps University Marburg
*--------------------------------------------------
use "$working_ANALYSIS\processed\si22.dta" , clear


*-----------------------------------
*   2.3:  Measurement variables
*-----------------------------------
*Fig 1. WTP elicitation
mylabels 0(5)25, myscale(@) local(pctlabel) suffix("%") 
hist wtp_wins, d percent xtitle(WTP in SBD) xla(0(20)200, nogrid) yla(`pctlabel', nogrid) ytitle("") fcolor(*0.9) lcolor(none) xline(67, lpattern(dash)) xsize(4) ysize(3) scale(1.1)
graph save "$working_ANALYSIS\results\intermediate\Figure1_WTP_dist", replace
graph export "$working_ANALYSIS\results\figures\Figure1_WTP_dist.tif", replace width(3650)




* Fig. 2.	Valuation of nature measures
*Panel A: Likert-based
stripplot rv3_1 rv3_2 rv3_3 rv3_4 rel_val rv4 rv5, xline(5.5) yla(1(1)5, nogrid) yscale(r(0.8 5.2)) xtitle("") msymbol(Dh) mcolor("100 143 255*0.8") variablelabels ytitle("score") vertical   jitter(2) bar boffset(0.15)  refline(lw(medium)  lc(gs10) lp(dash))  reflinestretch(0.2)  xla(, noticks) yla(, ang(h))  title("{bf: A: } Likert Scales")
graph save "$working_ANALYSIS\results\intermediate\values_likert", replace
ttest rel_val = rv4
ttest rel_val = rv5

*Panel B: Ranking item
stripplot instrumental intrinsic relational_culture relational_steward avg_relational, xline(5.5) yla(1(1)4, nogrid) yscale(r(0.8 4.2)reverse) xtitle("") msymbol(Dh) mcolor("100 143 255*0.8") variablelabels ytitle("rank") vertical   jitter(2) bar boffset(0.15)  refline(lw(medium)  lc(gs10) lp(dash))  reflinestretch(0.2)  xla(, noticks) yla(, ang(h))  title("{bf: B: } Ranking")
graph save "$working_ANALYSIS\results\intermediate\values_ranking", replace
ttest instrumental = intrinsic
ttest instrumental = relational_culture
ttest instrumental = relational_steward
ttest instrumental = avg_relational
ttest intrinsic = avg_relational
gr combine "$working_ANALYSIS\results\intermediate\values_likert"  "$working_ANALYSIS\results\intermediate\values_ranking", xsize(3.465) ysize(3) cols(1) graphregion(margin(0 0 0 0)) plotregion(margin(0 0 0 0)) 
graph save "$working_ANALYSIS\results\intermediate\rvs_catplot", replace
graph export "$working_ANALYSIS\results\figures\Figure2_relational_values.tif", replace width(3650)




* Fig. 3.	Budget
* Government budget allocation
mylabels 0(5)20, myscale(@) local(pctlabel) suffix("%") 
hist allocation_percent_wins, d percent xtitle(Increase in pp) xla(0(5)30, nogrid) yla(`pctlabel', nogrid) ytitle("") fcolor(*0.9) lcolor(none) xline(67, lpattern(dash)) xsize(4) ysize(3) scale(1.1)
graph save "$working_ANALYSIS\results\intermediate\Figure3_budget_dist", replace
graph export "$working_ANALYSIS\results\figures\Figure3_budget_dist.tif", replace width(3650)

sum allocation_percent_wins, detail

*-------------------------------------------
*   2.4:  DATA
*------------------------------------------
// Table 1: Summary and balance table (Pooled | Control | Treat)
global summary wtp_wins rel_val avg_relational allocation_percent_wins /// Outcome measures
atoll_tie /// Ties
female age single hh_income_average ln_income edu1 edu2 edu3 // Socio-Demographcis

* Summary table
estpost tabstat $summary, statistics(count mean median sd min max) columns(statistics)
esttab . using "$working_ANALYSIS/results/tables/Table1_summary.rtf", cells("mean(fmt(%9.2fc)) sd(fmt(%9.2fc))") compress not nostar unstack nomtitle nonumber nonote replace label

* Balance table
generate t1 = (treat==1)
balancetable (mean if treat==0) (mean if treat==1) (diff t1 if treat!=2) $summary using "$working_ANALYSIS/results/tables/Table1_balance.xlsx", wide sheet(First sheet) cell(A1) ctitles("Mean" "SD" "Mean" "SD" "Diff" "SE") format(%4.2f) nopar replace  
ttest ties_cat2, by(treat)

*joint f-test of orthoganility
reg treat atoll_tie female age single hh_income_average i.edu, robust




*---------------------------
* 3.1 Treatment Effects
*---------------------------
*Fig. 4.	Average and heterogeneous treatment effects
global sociodem age female single hh_income_average_w edu2 edu3

*Panel A: WTP
eststo wtp1: reg wtp_wins i.treat , vce(hc3)
estadd local controls "No"
eststo wtp2: reg wtp_wins i.treat $sociodem, vce(hc3)
estadd local controls "Yes"
eststo wtp3: reg wtp_wins i.treat##i.atoll_tie, vce(hc3)
estadd local controls "No"
eststo wtp4: reg wtp_wins i.treat##i.atoll_tie $sociodem, vce(hc3)
estadd local controls "Yes"
margins, dydx(treat) at(atoll_tie=(0 1))
*income effect : For every 500 SBD increase in household income, participants are predicted to to contribute 2.7 SBD more to the protection fund:
margins, dydx() at(hh_income_average_w=(1000(500)3000))
di .007979*500


*Panel B: Relational Values Likert index
eststo rv1: reg z_rel_val i.treat, vce(hc3)
estadd local controls "No"
eststo rv2: reg z_rel_val i.treat $sociodem , vce(hc3)
estadd local controls "Yes"
eststo rv3: reg z_rel_val i.treat##i.atoll_tie, vce(hc3)
estadd local controls "No"
eststo rv4: reg z_rel_val i.treat##i.atoll_tie $sociodem , vce(hc3)
estadd local controls "Yes"
margins, dydx(treat) at(atoll_tie=(0 1))

*Panel C: Average rank relational values (combined culture and steward), lower value implies higer rank in terms of importance of conservation
eststo rank1: reg avg_relational i.treat, vce(hc3)
estadd local controls "No"
eststo rank2: reg avg_relational i.treat $sociodem , vce(hc3)
estadd local controls "Yes"
eststo rank3: reg avg_relational i.treat##i.atoll_tie, vce(hc3)
estadd local controls "No"
eststo rank4: reg avg_relational i.treat##i.atoll_tie $sociodem , vce(hc3)
estadd local controls "Yes"
margins, dydx(treat) at(atoll_tie=(0 1))

*Panel D: Increase in budget spending for envrionment and climate change management
eststo budget1: reg allocation_percent_wins i.treat, vce(hc3)
estadd local controls "No"
eststo budget2: reg allocation_percent_wins i.treat $sociodem , vce(hc3)
estadd local controls "Yes"
eststo budget3: reg allocation_percent_wins i.treat##i.atoll_tie, vce(hc3)
estadd local controls "No"
eststo budget4: reg allocation_percent_wins i.treat##i.atoll_tie $sociodem , vce(hc3)
estadd local controls "Yes"
margins, dydx(treat) at(atoll_tie=(0 1))

coefplot(wtp2,  label(ATE)) (wtp4, offset(-0.3)  label(Interaction model)), xsc(r(-40 (10) 40)) bylabel({bf:A} WTP (in SBD)) || (rv2, label(ATE)) (rv4,label(Interaction model)), xsc(r(-0.5 0.5)) bylabel({bf:B} Likert Index (in SD)) || (rank2,  label(ATE)) (rank4, label(Interaction model)),  xsc(r(-0.5 0.5)) bylabel("{bf:C} Average Rank:" "Culture&Stewardship") || (budget2, label(ATE)) (budget4,  label(Interaction model)),  xsc(r(-5 5)) bylabel("{bf:D} Budget spending" "(in pp)") ||,  keep(1.treat 1.atoll_tie 1.treat#1.atoll_tie) coeflabels(1.treat = "Treated" 1.atoll_tie ="Atoll connection" 1.treat#1.atoll_tie="Treated*Atoll connection" , labsize(6pt))  byopts(compact xrescale rows(2)) xline(0, lpattern(dash) lcolor(gs3)) xtitle("Regression estimated impact relative to control", size(6pt)) grid(none) levels(95 90) ciopts(lwidth(0.8 2)  lcolor(*1 *.3) recast(rcap)) mlabel(cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", "")))) msize(3pt) msymbol(D) mlabsize(10pt) mlabposition(2) subtitle(, size(9pt) lstyle(none) margin(medium) nobox justification(center) alignment(top) bmargin(top))  xsize(3.465) ysize(3.465)  
gr_edit b1title.DragBy -.2181025081788391 13.08615049073066
gr_edit plotregion1.xaxis1[1].reset_rule 5, tickset(major) ruletype(suggest) 
gr_edit plotregion1.xaxis1[2].reset_rule 5, tickset(major) ruletype(suggest) 
gr_edit style.editstyle margin(vsmall) editcopy
gr save  "$working_ANALYSIS\results\intermediate\figure4_treatment_effects.gph", replace
gr export "$working_ANALYSIS\results\figures\figure4_treatment_effects.tif", replace width(4000)




*------------------------------------------------------------------------------
* 3.2	Correlation between WTP and budget allocation with relational values  
*------------------------------------------------------------------------------
**Figure 5. Correlation monetary measures with non-monetary measures
global controls age female single edu2 edu3 hh_member hh_income_average_w wealth_pca 

*Panel A: raw data correlation
twoway (scatter share_wtp_wins rel_val [fweight=count_rel_val],  mcolor(*.3) mlcolor(none)) (lpoly share_wtp_wins rel_val [fweight=count_rel_val],  yla(0(20)100, nogrid) lwidth(medthick) msize(medlarge) mcolor(*.3)  bwidth(0.6)), legend(off)  title("{bf: A:} WTP (raw data)") xtitle("Relational values index") ytitle("in SBD") graphregion(fcolor(none) lwidth(none))
gr save  "$working_ANALYSIS\results\intermediate\figure5_a.gph", replace


*Panel B: variation at censored values (3 categories)
stripplot wtp_wins, over(rel_cat) yla(0(40)200, nogrid)  xtitle("") msymbol(Dh) mcolor("100 143 255*0.8") variablelabels ytitle("Donation amount in SBD") vertical  jitter(2) bar boffset(0.15)  refline(lw(medium)  lc(gs10) lp(dash))  reflinestretch(0.1)  xla(, noticks) yla(, ang(h))  title("{bf: B: } WTP Variation by Index") graphregion(color(none))
graph save "$working_ANALYSIS\results\intermediate\figure5_b", replace

*Panel C:

pwcorr allocation_percent_wins rel_val, sig
 
twoway (scatter share_allocation_wins rel_val [fweight=count_allo],  mcolor(*.3) mlcolor(none)) (lpoly share_allocation_wins rel_val [fweight=count_allo],  yla(0(5)20, nogrid) lwidth(medthick) msize(medlarge) mcolor(*.3)  bwidth(0.6)), legend(off)  title("{bf: C:} Budget increases (raw data)") xtitle("Relational values index") ytitle("in pp") graphregion(fcolor(none) lwidth(none))
gr save  "$working_ANALYSIS\results\intermediate\figure5_c.gph", replace

*Panel D: variation at censored values (3 categories)
stripplot allocation_percent_wins, over(rel_cat) yla(0(5)30, nogrid)  xtitle("") msymbol(Dh) mcolor("100 143 255*0.8") variablelabels ytitle("Budget increase in pp") vertical  jitter(2) bar boffset(0.15)  refline(lw(medium)  lc(gs10) lp(dash))  reflinestretch(0.1)  xla(, noticks) yla(, ang(h))  title("{bf: D: } Budget Variation by Index")  graphregion(color(none))
graph save "$working_ANALYSIS\results\intermediate\figure5_d", replace

gr combine "$working_ANALYSIS\results\intermediate\figure5_a"  "$working_ANALYSIS\results\intermediate\figure5_b"  "$working_ANALYSIS\results\intermediate\figure5_c.gph" "$working_ANALYSIS\results\intermediate\figure5_d.gph", xsize(3) ysize(2) cols(2) scale(1.15)
graph save "$working_ANALYSIS\results\intermediate\figure5_correlation_outcomes", replace
graph export "$working_ANALYSIS\results\figures\figure5_correlation_outcomes.tif", replace width(3650)


*regressions
ttest wtp_wins if rel_cat!=1, by(rel_cat)
ttest allocation_percent_wins if rel_cat!=1, by(rel_cat)

eststo wtp_det1: reg wtp_wins  i.rel_cat   , vce(robust)
eststo wtp_det2: reg wtp_wins  i.rel_cat   $controls, vce(robust)
eststo budget_det1: reg allocation_percent_wins  i.rel_cat   , vce(robust)
eststo budget_det2: reg allocation_percent_wins  i.rel_cat   $controls, vce(robust)




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


*Table S2.	Main effects: WTP
esttab wtp1 wtp2 wtp3 wtp4 using "$working_ANALYSIS\results\tables\tableS2_treat_wtp_intensive.rtf", keep(1.treat 1.atoll_tie 1.treat#1.atoll_tie  $sociodem _cons) order(1.treat 1.atoll_tie 1.treat#1.atoll_tie  $sociodem _cons) label se(%4.2f) transform(ln*: exp(@) exp(@))  mgroups("ATE" "Heterogeneous effects", pattern(1 0 1 0)) nomtitles b(%4.2f) stats(controls N r2_a , labels("Controls" "N" "Adjusted R-squared") fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(,elist(weight:_cons "{break}{hline @width}"))nonotes  addnotes("Notes: Robust standard errors in brackets: * p < 0.10, ** p < 0.05, *** p < 0.01.") replace


*Table S3.	Main effects: Relational values Likert index
esttab rv1 rv2 rv3 rv4 using "$working_ANALYSIS\results\tables\tableS3_treat_rv_likert.rtf", keep(1.treat 1.atoll_tie 1.treat#1.atoll_tie  $sociodem _cons) order(1.treat 1.atoll_tie 1.treat#1.atoll_tie  $sociodem _cons)  label se(%4.2f) transform(ln*: exp(@) exp(@))  mgroups("ATE" "Heterogeneous effects", pattern(1 0 1 0)) nomtitles b(%4.2f) stats(controls N r2_a , labels("Controls" "N" "Adjusted R-squared") fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(,elist(weight:_cons "{break}{hline @width}"))nonotes  addnotes("Notes: Robust standard errors in brackets: * p < 0.10, ** p < 0.05, *** p < 0.01.") replace


*Table S4.	Main effects: Relational values Ranking
esttab rank1 rank2 rank3 rank4 using "$working_ANALYSIS\results\tables\tableS4_treat_rv_ranking.rtf", keep(1.treat 1.atoll_tie 1.treat#1.atoll_tie  $sociodem _cons) order(1.treat 1.atoll_tie 1.treat#1.atoll_tie  $sociodem _cons)  label se(%4.2f) transform(ln*: exp(@) exp(@))  mgroups("ATE" "Heterogeneous effects", pattern(1 0 1 0)) nomtitles b(%4.2f) stats(controls N r2_a , labels("Controls" "N" "Adjusted R-squared") fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(,elist(weight:_cons "{break}{hline @width}"))nonotes  addnotes("Notes: Robust standard errors in brackets: * p < 0.10, ** p < 0.05, *** p < 0.01.") replace


*Table S5.	Main effects: Government budget allocation
esttab budget1  budget2 budget3 budget4 using "$working_ANALYSIS\results\tables\tableS5_treat_budget.rtf", keep(1.treat 1.atoll_tie 1.treat#1.atoll_tie  $sociodem _cons) order(1.treat 1.atoll_tie 1.treat#1.atoll_tie  $sociodem _cons)  label se(%4.2f) transform(ln*: exp(@) exp(@))  mgroups("ATE" "Heterogeneous effects", pattern(1 0 1 0)) nomtitles b(%4.2f) stats(controls N r2_a , labels("Controls" "N" "Adjusted R-squared") fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(,elist(weight:_cons "{break}{hline @width}"))nonotes  addnotes("Notes: Robust standard errors in brackets: * p < 0.10, ** p < 0.05, *** p < 0.01.") replace


* Table S6.	Correlation relational values index and WTP at the extensive and intensive margin
esttab wtp_det1 wtp_det2 budget_det1 budget_det2 using "$working_ANALYSIS\results\tables\tableS6_corr_RV_WTP.rtf", label se(%4.2f) transform(ln*: exp(@) exp(@))  mgroups("Amount donated in SBD" "Budget increase in pp", pattern(1 0 1 0)) nomtitles b(%4.2f) stats(controls N r2_a , labels("Controls" "N" "Adjusted R-squared") fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(,elist(weight:_cons "{break}{hline @width}"))nonotes  addnotes("Notes: All estimates are from ordinary least square regressions. Robust standard errors in brackets: * p < 0.10, ** p < 0.05, *** p < 0.01.") replace


* Table S7.	Extrapolating WTP to general population in Solomon Islands
tobit wtp_wins i.ethnicity rel_val  $controls, vce(robust) ll(0) ul(200)
margins, at(ethnicity = (1 2 3))
*extrapolations mainly done in excel file called "extrapolation.xlsx"

* Table S8.	Extrapolating budet allocation to general population in Solomon Islands
tobit allocation_percent_wins i.ethnicity rel_val  $controls, vce(robust) ll(1) ul(30)
margins, at(ethnicity = (1 2 3))
*extrapolations mainly done in excel file called "extrapolation.xlsx"




** EOF