/* Source: auc_roc_lasso.sas (model-1 slice; analysis logic unchanged).
   Changes from upstream:
     - The PROC IMPORT of the external CSV and the merge with the external
       pha.test_sample are replaced by the inline sample1_lasso_pp_test
       supplied in autoexec.sas (same three columns the pipeline reads:
       pred_prob, weight1, outcome_6msa).
     - The ROC output dataset is written to work (roc_sample1_lasso_test)
       instead of the author's external harvrd04 libname.
     - The final ODS EXCEL / ODS HTML destinations are pointed at relative
       files.
   Everything else -- the weighted-ROC PROC LOGISTIC (nofit, roc pred=),
   the PROC UNIVARIATE percentile output (pctlpts = 1 to 100 by 1), the
   ventile categorization DATA step, and the PROC SURVEYFREQ crosstabs --
   is exactly the author's model-1 code. */

*lasso;
*base model 1;
proc logistic data=sample1_lasso_pp_test rocoptions(weighted);
	weight weight1;
	model outcome_6msa (event='1')=/nofit outroc=roc_sample1_lasso_test;
	roc pred=pred_prob;
	ods output rocassociation=auc_sample1_lasso_test;
run;
data roc_sample1_lasso_test;
	set roc_sample1_lasso_test;
	format _sensit_ 7.6 _1mspec_ 7.6;
run;

*lasso;
*base model 1;
data sample1_lasso_pp_test;
	set sample1_lasso_pp_test;
id=1;

run;
proc univariate data=sample1_lasso_pp_test;
	weight weight1;
	var pred_prob;
	output out=pSL_cut_points pctlpts =1 to 100 by 1 pctlpre = total;
run;
data pSL_cut_points;
	set pSL_cut_points;
	id=1;
run;
data pp_lasso1;
	merge 	sample1_lasso_pp_test
			pSL_cut_points;
	by id;

	if pred_prob ge total95 then ventile_cat=1;
	else if pred_prob ge total90 then ventile_cat=2;
	else if pred_prob ge total85 then ventile_cat=3;
	else if pred_prob ge total80 then ventile_cat=4;
	else if pred_prob ge total75 then ventile_cat=5;
	else if pred_prob ge total70 then ventile_cat=6;
	else if pred_prob ge total65 then ventile_cat=7;
	else if pred_prob ge total60 then ventile_cat=8;
	else if pred_prob ge total55 then ventile_cat=9;
	else if pred_prob ge total50 then ventile_cat=10;
	else if pred_prob ge total45 then ventile_cat=11;
	else if pred_prob ge total40 then ventile_cat=12;
	else if pred_prob ge total35 then ventile_cat=13;
	else if pred_prob ge total30 then ventile_cat=14;
	else if pred_prob ge total25 then ventile_cat=15;
	else if pred_prob ge total20 then ventile_cat=16;
	else if pred_prob ge total15 then ventile_cat=17;
	else if pred_prob ge total10 then ventile_cat=18;
	else if pred_prob ge total5 then ventile_cat=19;
	else ventile_cat=20;

	if ventile_cat le 2 then ventile_cat2=1;
	if ventile_cat le 3 then ventile_cat3=1;
	if ventile_cat le 4 then ventile_cat4=1;
	if ventile_cat le 5 then ventile_cat5=1;

run;

ods excel file = "lasso_6msa_aucroc.xlsx";
proc print data=auc_sample1_lasso_test;
title "lasso model1: 5 suicidality predictors + age/sex/rank";
run;
ods excel close;

ods html file = "lasso_6msa_ctab.html";
proc surveyfreq data=pp_lasso1 missing;
title "lasso model1: 5 suicidality predictors + age/sex/rank";
weight weight1;
	tables ventile_cat*outcome_6msa/row col;
	tables ventile_cat2*outcome_6msa/ row col ;
	tables ventile_cat3*outcome_6msa/ row col ;
	tables ventile_cat4*outcome_6msa/ row col ;
	tables ventile_cat5*outcome_6msa/ row col ;
run;
ods html close;
