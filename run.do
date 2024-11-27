*-----------------------------------------------------------------------------------------------------------
* This package replicates the analysis reported in "Quantifying Relational Values in the Context of Adaptation in Atoll Ecosystems" prepared for the special issue "Relational Turn In Sustainability" @Ecosystems & People.

* Authors: Marco Nilgen, Maximilian Nicolaus Burger, Ivo Steimanis, Björn Vollan*

* Affiliations: 1 Research Group for Sustainable Use of Natural Resources, Philipps University Marburg, 35032 Marburg, Germany

* Correspondence: *Research Group for Sustainable Use of Natural Resources, School of Business and Economics, Philipps-Universität Marburg, Am Plan 1, D-35032 Marburg, Germany, Tel: +49-6421 2823725

* ORCID: Nilgen: 0000-0001-7358-4965; Burger: 0000-0003-2334-3885; Steimanis: 0000-0002-8550-4675; Vollan: 0000-0002-5592-4185

*Keywords: contingent valuation, relational values, monetary valuation, experiment, Atoll ecosystems
*-----------------------------------------------------------------------------------------------------------

*--------------------------------------------------
* Set global Working Directory
*--------------------------------------------------
* Define this global macro to point where the replication folder is saved locally that includes this run.do script

global working_ANALYSIS "YourPath"


*--------------------------------------------------
* Program Setup
*--------------------------------------------------
* Initialize log and record system parameters
clear
set more off
cap mkdir "$working_ANALYSIS/scripts/logs"
cap log close
local datetime : di %tcCCYY.NN.DD!-HH.MM.SS `=clock("$S_DATE $S_TIME", "DMYhms")'
local logfile "$working_ANALYSIS/scripts/logs/`datetime'.log.txt"
log using "`logfile'", text

di "Begin date and time: $S_DATE $S_TIME"
di "Stata version: `c(stata_version)'"
di "Updated as of: `c(born_date)'"
di "Variant:       `=cond( c(MP),"MP",cond(c(SE),"SE",c(flavor)) )'"
di "Processors:    `c(processors)'"
di "OS:            `c(os)' `c(osdtl)'"
di "Machine type:  `c(machine_type)'"

*   Analyses were run on Windows using Stata version 16
version 16              // Set Version number for backward compatibility

* All required Stata packages are available in the /libraries/stata folder
tokenize `"$S_ADO"', parse(";")
while `"`1'"' != "" {
  if `"`1'"'!="BASE" cap adopath - `"`1'"'
  macro shift
}
adopath ++ "$working_ANALYSIS/scripts/libraries/stata"
mata: mata mlib index
sysdir set PERSONAL "$working_ANALYSIS/scripts/libraries/stata"

* Create directories for output files
cap mkdir "$working_ANALYSIS/processed"
cap mkdir "$working_ANALYSIS/results"
cap mkdir "$working_ANALYSIS/results/intermediate"
cap mkdir "$working_ANALYSIS/results/tables"
cap mkdir "$working_ANALYSIS/results/figures"
* -------------------------------------------------

* Set general graph style
set scheme swift_red //select one scheme as reference scheme to work with
grstyle init 
{
*Background color
grstyle set color white: background plotregion graphregion legend box textbox //

*Main colors (note: swift_red only defines 8 colors. Multiplying the color, that is "xx yy zz*0.5" reduces/increases intensity and "xx yy zz%50" reduces transparency)
grstyle set color 	"100 143 255" "120 94 240" "220 38 127" "254 97 0" "255 176 0" /// 5 main colors
					"100 143 255*0.4" "120 94 240*0.4" "220 38 127*0.4" "254 97 0*0.4" "255 176 0*0.4" ///
					"100 143 255*1.7" "120 94 240*1.7" "220 38 127*1.7" "254 97 0*1.7" "255 176 0*1.7" ///
					: p# p#line p#lineplot p#bar p#area p#arealine p#pie histogram 

*Font size
grstyle set size 10pt: heading //titles
grstyle set size 8pt: subheading axis_title //axis titles
grstyle set size 8pt: p#label p#boxlabel body small_body text_option axis_label tick_label minortick_label key_label //all other text
}
* -------------------------------------------------

*--------------------------------------------------
* Run processing and analysis scripts
*--------------------------------------------------
do "$working_ANALYSIS\scripts\1_clean.do"
do "$working_ANALYSIS\scripts\2_analysis.do"

* End log
di "End date and time: $S_DATE $S_TIME"
log close
 
 
 
** EOF
