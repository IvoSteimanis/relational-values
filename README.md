# Replication Package
This repository contains the raw data and code that replicates tables and figures for the following paper: <br><br>
__Title:__ Pitfalls of Monetizing Relational Values in the Context of Climate Change Adaptation <br>
__Authors:__ Marco Nilgen, Maximilian Nicolaus Burger, Ivo Steimanis, Björn Vollan<sup>*</sup> <br>
__Affiliations:__ All Research Group for Sustainable Use of Natural Resources, Philipps University Marburg, 35032 Marburg, Germany  <br>
__*Correspondence to:__ Bjoern Vollan bjoern.vollan@wiwi.uni-marburg.de <br>
__ORCID:__ Nilgen: 0000-0001-7358-4965; Burger: 0000-0003-2334-3885; Steimanis: 0000-0002-8550-4675; Vollan: 0000-0002-5592-4185 <br>
__Keywords:__ contingent valuation, relational values, monetary valuation, experiment, Atoll ecosystems <br>

## Abstract
Relational values emphasize the desirable characteristics of nature-society relationships. Unlike instrumental values, relational values have not yet been subjected to monetary quantification, although they may be relevant to environmental policymaking or climate change adaptation decisions which often rely on cost-benefit approximations. This paper explores the quantification of relational values within a contingent valuation scenario both in monetary (one-time donation) and non-monetary terms (Likert-scale, ranking) as well as using a measure that elicits the desired allocation of government budget for adaptation. We conduct two surveys within the context of adaptation projects, aiming to protect the traditional lifestyles of atoll islanders on the Solomon Islands and coastal communities in Bangladesh. In these surveys, we employ two valuation scenarios—one with explicit mention of relational value losses, and one without. Information on relational losses led to no increases in monetary or non-monetary valuation but to a slightly higher allocation of government budget in Bangladesh. We further assess and discuss the validity of our measures, also accounting for respondents’ financial situation. Our findings suggest that emphasizing relational losses could significantly increase disaster management funding in Bangladesh, with a potential 55% budget increase based on our treatment effect. We further discuss the difficulties in quantifying relational values in a context with limited ability to pay and the importance of considering deliberative approaches for ensuring that all dimensions of human-nature relationships are adequately considered in adaptation policy decision-making.
## License
The data and code are licensed under a Creative Commons Attribution 4.0 International Public License. See __LICENSE.txt__ for details.

## Software requirements
All analysis were done in Stata version 16:
- Add-on packages are included in __scripts/libraries/stata__ and do not need to be installed by user. The names, installation sources, and installation dates of these packages are available in __scripts/libraries/stata/stata.trk__.

## Instructions
1.	Save the folder __‘replication_package’__ to your local drive.
2.	Open the master script __‘run.do’__ and change the global pointing to the working direction (line 20) to the location where you save the folder on your local drive 
3.	Run the master script __‘run.do’__  to replicate the analysis and generate all tables and figures reported in the paper and supplementary online materials

## Datasets
- The anonymized survey data are stored in : __‘Honiara_2022_raw.xlsx’__

## Descriptions of scripts
__scripts/01_clean__ <br>
This script processes the raw survey data and prepares them for analysis. <br>
__scripts/02_analysis.do__ <br>
This script creates figures and tables shown in the paper and supplementary materials and saves them to __results/figures and results/tables__ <br>





